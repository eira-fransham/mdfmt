#![feature(proc_macro_hygiene, or_patterns)]

use auto_enums::auto_enum;
use itertools::Itertools;
use pulldown_cmark::{CodeBlockKind, CowStr, Event, LinkType, Parser, Tag};
use std::{
    borrow::Cow,
    collections::HashMap,
    fmt::{self, Write as _},
    fs, io, iter, mem,
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
        Tag::Item | Tag::Link(..) | Tag::Emphasis | Tag::Strikethrough | Tag::Strong => {
            unreachable!()
        }
        Tag::List(_) => None,
        Tag::CodeBlock(CodeBlockKind::Indented)
        | Tag::BlockQuote
        | Tag::Paragraph
        | Tag::Heading(_) => None,
        other => unimplemented!("{:?}", other),
    }
}

fn tag_close(tag: &Tag) -> Cow<'static, str> {
    match tag {
        Tag::CodeBlock(CodeBlockKind::Fenced(_)) => format!("```").into(),
        Tag::CodeBlock(CodeBlockKind::Indented)
        | Tag::Item
        | Tag::List(_)
        | Tag::BlockQuote
        | Tag::Paragraph
        | Tag::Heading(_) => "".into(),
        other => unimplemented!("{:?}", other),
    }
}

// TODO: We don't need to allocate a string for `Heading`, we can have a `Len` trait + `fmt::Display`
fn tag_line_start(tag: &Tag) -> Cow<'static, str> {
    match tag {
        Tag::BlockQuote => "> ".into(),
        Tag::Paragraph => "".into(),
        Tag::List(_) => "".into(),
        Tag::Item => "  ".into(),
        Tag::CodeBlock(CodeBlockKind::Fenced(_)) => "".into(),
        Tag::CodeBlock(CodeBlockKind::Indented) => "    ".into(),
        // TODO: Allow non-ATX headings
        Tag::Heading(level) => match level {
            0 => unreachable!(),
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
        other => unimplemented!("{:?}", other),
    }
}

pub fn format<'a, 'b: 'a, I: IntoIterator<Item = Event<'a>> + 'a>(
    iter: I,
    width: usize,
    min_width: usize,
    break_long_words: bool,
    links_to_emit: &'b mut HashMap<String, String>,
) -> impl Iterator<Item = Event<'a>> + 'a {
    let mut text_buffer: Option<String> = None;
    let mut indent = 0;
    let mut is_code_section = false;

    let wrap_text_buffer = move |text_buffer: Option<String>, indent: usize| {
        text_buffer.into_iter().flat_map(move |buf| {
            let mut wrapper = Wrapper::new(width.saturating_sub(indent).max(min_width));
            wrapper.break_words = break_long_words;

            let vec = wrapper
                .into_wrap_iter(&buf)
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
            Event::Start(Tag::Link(linktype, target, _)) => {
                if let LinkType::Reference(rname) = linktype {
                    links_to_emit.insert(rname.to_string(), target.to_string());
                }

                match &mut text_buffer {
                    Some(buf) => buf.push_str("["),
                    None => text_buffer = Some("[".to_string()),
                }

                Box::new(iter::empty())
            }
            Event::Start(Tag::Emphasis) => {
                match &mut text_buffer {
                    Some(buf) => buf.push_str("_"),
                    None => text_buffer = Some("_".to_string()),
                }

                Box::new(iter::empty())
            }
            Event::Start(Tag::Strong) => {
                match &mut text_buffer {
                    Some(buf) => buf.push_str("**"),
                    None => text_buffer = Some("**".to_string()),
                }

                Box::new(iter::empty())
            }
            Event::Start(tag) => {
                let text_iter = match &tag {
                    Tag::CodeBlock(_) => {
                        assert!(!is_code_section);
                        is_code_section = true;

                        Some(
                            wrap_text_buffer(text_buffer.take(), indent)
                                .chain(Some(Event::SoftBreak)),
                        )
                        .into_iter()
                        .flatten()
                    }
                    Tag::List(_) => Some(wrap_text_buffer(text_buffer.take(), indent).chain(None))
                        .into_iter()
                        .flatten(),
                    _ => None.into_iter().flatten(),
                };

                indent += tag_line_start(&tag).len();

                Box::new(text_iter.chain(iter::once(Event::Start(tag))))
            }
            Event::End(Tag::Link(LinkType::Reference(rname), target, title)) => {
                match &mut text_buffer {
                    Some(buf) => {
                        buf.push_str("][");
                        buf.push_str(&*rname);
                        buf.push_str("]");
                    }
                    None => text_buffer = Some(format!("][{}]", rname)),
                }

                Box::new(iter::empty())
            }
            Event::End(Tag::Emphasis) => {
                match &mut text_buffer {
                    Some(buf) => buf.push_str("_"),
                    None => text_buffer = Some("_".to_string()),
                }

                Box::new(iter::empty())
            }
            Event::End(Tag::Strong) => {
                match &mut text_buffer {
                    Some(buf) => buf.push_str("**"),
                    None => text_buffer = Some("**".to_string()),
                }

                Box::new(iter::empty())
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

pub fn display<'a, 'b, I: IntoIterator<Item = Event<'a>> + 'a, W: io::Write>(
    iter: I,
    width: usize,
    min_width: usize,
    f: &'b mut W,
) -> io::Result<()> {
    let mut tag_stack = vec![];
    let mut in_end_state = false;
    let mut has_emitted_newlines = true;
    let mut emit_softbreak = false;

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
            Event::Start(Tag::Item) => {
                let start_num = match tag_stack.pop() {
                    Some(Tag::List(start_num)) => start_num,
                    _ => unreachable!(),
                };

                if !has_emitted_newlines {
                    writeln!(f)?;

                    for tag in &mut tag_stack {
                        write!(f, "{}", tag_line_start(&*tag))?;
                    }

                    has_emitted_newlines = true;
                }

                match start_num {
                    Some(val) => write!(f, "{}. ", val)?,
                    None => write!(f, "- ")?,
                }

                tag_stack.push(Tag::List(start_num.map(|i| i + 1)));
                tag_stack.push(Tag::Item);
            }
            Event::Start(tag) => {
                if emit_softbreak {
                    if !has_emitted_newlines {
                        writeln!(f)?;

                        for tag in &mut tag_stack {
                            write!(f, "{}", tag_line_start(&*tag))?;
                        }
                    }

                    emit_softbreak = false;
                }

                if let Some(open) = tag_open(&tag) {
                    writeln!(f, "{}", open)?;
                } else {
                    write!(f, "{}", tag_line_start(&tag))?;
                }

                tag_stack.push(tag);
            }
            Event::End(Tag::Item) => {
                tag_stack.pop().unwrap();
            }
            Event::End(tag) => {
                tag_stack.pop().unwrap();

                for tag in &mut tag_stack {
                    write!(f, "{}", tag_line_start(&*tag))?;
                }

                write!(f, "{}", tag_close(&tag))?;

                in_end_state = true;
            }
            Event::Text(text) => {
                has_emitted_newlines = false;

                let text = if text.starts_with("- ") {
                    write!(f, " -")?;
                    (&text[2..]).into()
                } else {
                    text
                };

                if emit_softbreak {
                    writeln!(f)?;

                    for tag in &mut tag_stack {
                        write!(f, "{}", tag_line_start(&*tag))?;
                    }

                    emit_softbreak = false;
                }

                write!(f, "{}", text)?
            }
            Event::SoftBreak => {
                emit_softbreak = true;
            }
            Event::HardBreak => emit_newlines!(),
            Event::Rule => {
                let mut indent = 0;

                for tag in &mut tag_stack {
                    let tag_line_start = tag_line_start(&*tag);
                    indent += tag_line_start.len();
                    write!(f, "{}", tag_line_start)?;
                }

                let num_dashes = width.saturating_sub(indent).max(min_width).max(3);

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

fn read_write<I: io::Read, O: io::Write>(
    mut input: I,
    mut output: O,
    opts: &Options,
) -> io::Result<()> {
    let mut input_string = String::new();
    input.read_to_string(&mut input_string)?;

    let mut links_to_emit = HashMap::new();

    display(
        format(
            Parser::new(&input_string),
            opts.width,
            opts.min_width,
            opts.break_long_words,
            &mut links_to_emit,
        ),
        opts.width,
        opts.min_width,
        &mut output,
    )?;

    let mut sorted_links = links_to_emit.keys().map(|v| &v[..]).collect::<Vec<_>>();
    sorted_links.sort_unstable();

    if !sorted_links.is_empty() {
        writeln!(output)?;

        for link in sorted_links {
            writeln!(output, "[{}]: {}", link, links_to_emit[link])?;
        }
    }

    Ok(())
}

fn main() {
    let opts = Options::from_args();

    match (&opts.input, &opts.output) {
        (Some(i), Some(o)) => read_write(
            fs::File::open(i).unwrap(),
            fs::File::open(o).unwrap(),
            &opts,
        ),
        (None, Some(o)) => read_write(io::stdin().lock(), fs::File::open(o).unwrap(), &opts),
        (Some(i), None) => read_write(fs::File::open(i).unwrap(), io::stdout().lock(), &opts),
        (None, None) => read_write(io::stdin().lock(), io::stdout().lock(), &opts),
    }
    .unwrap()
}

#[cfg(test)]
mod test {
    use super::{display, format, Options};
    use std::{collections::HashMap, iter};
    use structopt::StructOpt;

    #[test]
    fn test() {
        use pulldown_cmark::Parser;

        let parser = Parser::new(
            r#"
Hello, world, asfkmas asf as faasfkmas asf as faasfkmas asf as faasfkmas asf as faasfkmas asf as faasfkmas asf as fa
one two three

> Here's a very long quote, asfkmas asf as faasfkmas asf as faasfkmas asf as faasfkmas asf as faasfkmas asf as faasfkmas asf as fa
> ...but there's more!
>
>
>

# Heading 1
## Heading 2
### Heading 3
#### Really really long heading, asfkmas asf as faasfkmas asf as faasfkmas asf as faasfkmas asf as faasfkmas asf as faasfkmas asf as fa

```test one two three
Some code! This should not be formatted at all, and should be allowed to be as long as it is in the original text! Don't split me!
```

--------

This is a test!
            "#,
        );

        let opts = &Options::from_iter(iter::empty::<std::ffi::OsString>());
        let mut links_to_emit = HashMap::new();
        display(
            format(
                parser,
                opts.width,
                opts.min_width,
                opts.break_long_words,
                &mut links_to_emit,
            ),
            opts.width,
            opts.min_width,
            &mut std::io::stdout().lock(),
        )
        .unwrap();
    }
}
