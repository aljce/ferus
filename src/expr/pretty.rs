use crate::expr::{Expr};

impl<'a> Expr<'a> {
    pub fn pretty(&self) -> String {
        fn draw<'a>(expr: &Expr<'a>, lines: &mut Vec<String>, cur: usize) -> usize {
            use Expr::*;
            match expr {
                Var(name) => {
                    lines.push(format!("{}", name));
                    cur + 1
                },
                Lit(lit) => {
                    lines.push(format!("{}", lit));
                    cur + 1
                },
                Unary{ operation, child } => {
                    lines.push(format!("{}", operation));
                    lines.push("│  ".to_string());
                    let bottom = draw(child, lines, cur + 2);
                    lines[cur + 2].insert_str(0, "└──");
                    for y in cur + 3 .. bottom {
                        lines[y].insert_str(0, "   ");
                    }
                    bottom
                },
                Binary{ left, operation, right } => {
                    lines.push(format!("{}", operation));
                    lines.push("│  ".to_string());
                    let top = draw(left, lines, cur + 2);
                    lines[cur + 2].insert_str(0, "├──");
                    for y in cur + 3 .. top {
                        lines[y].insert_str(0, "│  ");
                    }
                    lines.push("│  ".to_string());
                    let bottom = draw(right, lines, top + 1);
                    lines[top + 1].insert_str(0, "└──");
                    for y in top + 2 .. bottom {
                        lines[y].insert_str(0, "   ");
                    }
                    bottom
                },
                IfThenElse{ condition, if_branch, else_branch } => {
                    lines.push("if".to_string());
                    lines.push("│  ".to_string());
                    let top = draw(condition, lines, cur + 2);
                    lines[cur + 2].insert_str(0, "├──");
                    for y in cur + 3 .. top {
                        lines[y].insert_str(0, "│  ");
                    }
                    lines.push("│  ".to_string());
                    let middle = draw(if_branch, lines, top + 1);
                    lines[top + 1].insert_str(0, "├──");
                    for y in top + 2 .. middle {
                        lines[y].insert_str(0, "│  ");
                    }
                    lines.push("│  ".to_string());
                    let bottom = draw(else_branch, lines, middle + 1);
                    lines[middle + 1].insert_str(0, "└──");
                    for y in middle + 2 .. bottom {
                        lines[y].insert_str(0, "   ");
                    }
                    bottom
                },
                Let{ name, binder, body } => {
                    lines.push(format!("let {}=", name));
                    lines.push("│  ".to_string());
                    let top = draw(binder, lines, cur + 2);
                    lines[cur + 2].insert_str(0, "├──");
                    for y in cur + 3 .. top {
                        lines[y].insert_str(0, "│  ");
                    }
                    lines.push("│  ".to_string());
                    let bottom = draw(body, lines, top + 1);
                    lines[top + 1].insert_str(0, "└──");
                    for y in top + 2 .. bottom {
                        lines[y].insert_str(0, "   ");
                    }
                    bottom
                },
                Lambda{ name, body } => {
                    lines.push(format!("fn {}", name));
                    lines.push("│  ".to_string());
                    let bottom = draw(body, lines, cur + 2);
                    lines[cur + 2].insert_str(0, "└──");
                    for y in cur + 3 .. bottom {
                        lines[y].insert_str(0, "   ");
                    }
                    bottom
                },
                App{ left, right } => {
                    lines.push("App".to_string());
                    lines.push("│  ".to_string());
                    let top = draw(left, lines, cur + 2);
                    lines[cur + 2].insert_str(0, "├──");
                    for y in cur + 3 .. top {
                        lines[y].insert_str(0, "│  ");
                    }
                    lines.push("│  ".to_string());
                    let bottom = draw(right, lines, top + 1);
                    lines[top + 1].insert_str(0, "└──");
                    for y in top + 2 .. bottom {
                        lines[y].insert_str(0, "   ");
                    }
                    bottom
                },
                _ => panic!("Pretty printing does not support new features"),
            }
        }
        let mut lines = vec![];
        draw(self, &mut lines, 0);
        lines.join("\n")
    }
}
