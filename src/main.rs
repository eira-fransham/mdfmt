#![feature(proc_macro_hygiene)]

use auto_enums::auto_enum;
use itertools::Itertools;
use pulldown_cmark::{CodeBlockKind, CowStr, Event, Tag};
use std::{
    borrow::Cow,
    fmt::{self, Write as _},
    io, iter, mem,
    path::PathBuf,
};
use structopt::StructOpt;
use textwrap::{NoHyphenation, Wrapper};

#[derive(Debug, StructOpt)]
pub struct Options {
    /// The maximum column width of the output markdown file.
    #[structopt(short, long, default_value = "80")]
    width: usize,
    /// The minimum width of a text field. If you have deeply nested elements, this will mean
    /// that if the LHS of a text field is more than `width - min_width` characters indented,
    /// the text will run over and the total width may be higher than `width`.
    #[structopt(short, long, default_value = "20")]
    min_width: usize,
    /// Whether to split words that are too long to fit on a single line. Off by default.
    #[structopt(short, long)]
    break_long_words: bool,
    /// Input file (default: stdin)
    #[structopt(short, long, parse(from_os_str))]
    input: Option<PathBuf>,
    /// Output file (default: stdout)
    #[structopt(short, long, parse(from_os_str))]
    output: Option<PathBuf>,
}

fn tag_open(tag: &Tag) -> Option<Cow<'static, str>> {
    match tag {
        Tag::CodeBlock(CodeBlockKind::Fenced(language)) => Some(format!("```{}", language).into()),
        Tag::CodeBlock(CodeBlockKind::Indented)
        | Tag::BlockQuote
        | Tag::Paragraph
        | Tag::Heading(_) => None,
        _ => unimplemented!(),
    }
}

fn tag_close(tag: &Tag) -> Cow<'static, str> {
    match tag {
        Tag::CodeBlock(CodeBlockKind::Fenced(_)) => format!("```").into(),
        Tag::CodeBlock(CodeBlockKind::Indented)
        | Tag::BlockQuote
        | Tag::Paragraph
        | Tag::Heading(_) => "".into(),
        _ => unimplemented!(),
    }
}

// TODO: We don't need to allocate a string for `Heading`, we can have a `Len` trait + `fmt::Display`
fn tag_line_start(tag: &Tag) -> Cow<'static, str> {
    match tag {
        Tag::BlockQuote => "> ".into(),
        Tag::Paragraph => "".into(),
        Tag::CodeBlock(CodeBlockKind::Fenced(_)) => "".into(),
        Tag::CodeBlock(CodeBlockKind::Indented) => "    ".into(),
        // TODO: Allow non-ATX headings
        Tag::Heading(level) => match level {
            0 => unimplemented!(),
            1 => "# ".into(),
            2 => "## ".into(),
            3 => "### ".into(),
            4 => "#### ".into(),
            5 => "##### ".into(),
            6 => "###### ".into(),
            level => iter::repeat('#')
                .take(*level as usize)
                .chain(iter::once(' '))
                .collect::<String>()
                .into(),
        },
        _ => unimplemented!(),
    }
}

pub fn format<'a, I: IntoIterator<Item = Event<'a>> + 'a>(
    iter: I,
    options: &'a Options,
) -> impl Iterator<Item = Event<'a>> + 'a {
    let mut text_buffer: Option<String> = None;
    let mut indent = 0;
    let mut is_code_section = false;

    let wrap_text_buffer = move |text_buffer: Option<String>, indent: usize| {
        text_buffer.into_iter().flat_map(move |buf| {
            let vec = Wrapper::new(options.width.saturating_sub(indent).max(options.min_width))
                .wrap_iter(&buf)
                .map(|i| i.to_string())
                .collect::<Vec<String>>();

            vec.into_iter()
                .map(|line| Event::Text(line.into()))
                .intersperse(Event::SoftBreak)
        })
    };

    let map_fn = move |ev| -> Box<dyn Iterator<Item = _>> {
        if is_code_section {
            if let Event::End(_) = ev {
                is_code_section = false;
            } else {
                return Box::new(iter::once(ev));
            }
        }

        match ev {
            Event::Start(tag) => {
                if let Tag::CodeBlock(_) = tag {
                    assert!(!is_code_section);
                    is_code_section = true;
                }

                indent += tag_line_start(&tag).len();
                Box::new(iter::once(Event::Start(tag)))
            }
            Event::End(tag) => {
                let new_indent = indent - tag_line_start(&tag).len();
                let iter =
                    wrap_text_buffer(text_buffer.take(), indent).chain(iter::once(Event::End(tag)));
                indent = new_indent;
                Box::new(iter)
            }
            Event::Text(s) => {
                match &mut text_buffer {
                    Some(buf) => buf.push_str(s.as_ref()),
                    None => text_buffer = Some(s.to_string()),
                }

                Box::new(iter::empty())
            }
            Event::SoftBreak => {
                match &mut text_buffer {
                    Some(buf) => buf.push_str(" "),
                    None => text_buffer = Some(" ".to_string()),
                }

                Box::new(iter::empty())
            }
            Event::HardBreak => {
                let iter = wrap_text_buffer(text_buffer.take(), indent)
                    .chain(iter::once(Event::HardBreak));
                Box::new(iter)
            }
            Event::Code(code) => {
                // TODO: This is a total hack but `Wrapper` doesn't support doing it any better
                match &mut text_buffer {
                    Some(buf) => write!(buf, "`{}`", code).unwrap(),
                    None => text_buffer = Some(format!("`{}`", code)),
                }

                Box::new(iter::empty())
            }
            Event::Rule => Box::new(iter::once(Event::Rule)),
            _ => unimplemented!(),
        }
    };

    iter.into_iter().flat_map(map_fn)
}

pub fn display<'a, I: IntoIterator<Item = Event<'a>> + 'a, W: io::Write>(
    iter: I,
    options: &Options,
    f: &mut W,
) -> io::Result<()> {
    let mut tag_stack = vec![];
    let mut in_end_state = false;
    let mut has_emitted_newlines = false;

    macro_rules! emit_newlines {
        () => {
            if !has_emitted_newlines {
                for _ in 0..2 {
                    writeln!(f)?;
                    for tag in &mut tag_stack {
                        write!(f, "{}", tag_line_start(&*tag))?;
                    }
                }

                has_emitted_newlines = true;
            }
        };
    }

    for ev in iter {
        if let Event::End(_) = ev {
        } else if in_end_state {
            emit_newlines!();
            in_end_state = false;
        }

        match ev {
            Event::Start(tag) => {
                match tag_open(&tag) {
                    Some(open) => {
                        for tag in &mut tag_stack {
                            write!(f, "{}", tag_line_start(&*tag))?;
                        }

                        writeln!(f, "{}", open)?;
                    }
                    None => {}
                }

                tag_stack.push(tag);

                if has_emitted_newlines {
                    for tag in &mut tag_stack {
                        write!(f, "{}", tag_line_start(&*tag))?;
                    }

                    has_emitted_newlines = false;
                }
            }
            Event::End(tag) => {
                let top_tag = tag_stack.pop().unwrap();

                assert_eq!(top_tag, tag);

                write!(f, "{}", tag_close(&tag))?;

                in_end_state = true;
            }
            Event::Text(text) => {
                has_emitted_newlines = false;

                write!(f, "{}", text)?
            }
            Event::Code(code) => {
                has_emitted_newlines = false;

                write!(f, "`{}`", code)?
            }
            Event::SoftBreak => {
                writeln!(f)?;

                for tag in &mut tag_stack {
                    write!(f, "{}", tag_line_start(&*tag))?;
                }
            }
            Event::HardBreak => emit_newlines!(),
            Event::Rule => {
                let mut indent = 0;

                for tag in &mut tag_stack {
                    let tag_line_start = tag_line_start(&*tag);
                    indent += tag_line_start.len();
                    write!(f, "{}", tag_line_start)?;
                }

                let num_dashes = options
                    .width
                    .saturating_sub(indent)
                    .max(options.min_width)
                    .max(3);

                for _ in 0..num_dashes {
                    write!(f, "-")?;
                }

                has_emitted_newlines = false;

                emit_newlines!()
            }
            _ => unimplemented!(),
        }
    }

    writeln!(f)?;

    Ok(())
}

fn main() {
    println!("Hello, world!");
}

#[cfg(test)]
mod test {
    use super::{display, format, Options};
    use std::iter;
    use structopt::StructOpt;

    #[test]
    fn test() {
        use pulldown_cmark::Parser;

        let parser = Parser::new(
            r#"
Hello, world, asfkmas asf as faasfkmas asf as faasfkmas asf as faasfkmas asf as faasfkmas asf as faasfkmas asf as fa

> Here's a very long quote, asfkmas asf as faasfkmas asf as faasfkmas asf as faasfkmas asf as faasfkmas asf as faasfkmas asf as fa
> ...but there's more!
>
>
>

# Heading 1
## Heading 2
### Heading 3
#### Really really long heading, asfkmas asf as faasfkmas asf as faasfkmas asf as faasfkmas asf as faasfkmas asf as faasfkmas asf as fa

```
Some code! This should not be formatted at all, and should be allowed to be as long as it is in the original text! Don't split me!
```

--------

This is a test!
            "#,
        );

        let opts = &Options::from_iter(iter::empty::<std::ffi::OsString>());
        display(format(parser, opts), opts, &mut std::io::stdout().lock()).unwrap();
    }
}
