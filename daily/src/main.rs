use std::process::Command;

const COMPLETE_SCRIPT : &'static str = r#"
tell application "Things3"
  repeat with toDo in to dos of list "Today"
    set toDoName to name of toDo
    if (toDoName as string) is equal to "Run Daily" then
      set completion date of toDo to current date
    end if
  end repeat
end tell
"#;

const ANKI_SCRIPT : &'static str = r#"
tell application "Anki"
  activate
end tell
tell application "System Events"
  keystroke "y"
  delay 3
  tell process "Anki"
    tell menu bar 1
      tell menu "Tools"
        click menu item "Check Media..."
      end tell
    end tell
  end tell
  delay 3
  keystroke "y"
end tell
"#;

fn main() {
    Command::new("osascript")
        .arg("-e")
        .arg(ANKI_SCRIPT)
        .status()
        .expect("failed to run Anki script");

    Command::new("open")
        .arg("/System/Library/CoreServices/Menu Extras/Bluetooth.menu")
        .status()
        .expect("failed to open bluetooth menu");

    Command::new("osascript")
        .arg("-e")
        .arg(COMPLETE_SCRIPT)
        .status()
        .expect("failed to check off 'Run Daily' task");

    Command::new("git")
        .arg("add")
        .arg("-A")
        .current_dir("/Users/jfelice/src/data")
        .status()
        .expect("failed to `git add -A` in data");

    Command::new("git")
        .arg("commit")
        .arg("-m")
        .arg("daily")
        .current_dir("/Users/jfelice/src/data")
        .status()
        .expect("failed to `git commit` in data");

    Command::new("git")
        .arg("push")
        .arg("origin")
        .arg("master")
        .current_dir("/Users/jfelice/src/data")
        .status()
        .expect("failed to `git push` in data");

    Command::new("gtypist")
        .arg("-b")
        .arg("/Users/jfelice/src/dotfiles/gtypist/gtypist.typ")
        .status()
        .expect("failed to `gtypist`");
}
