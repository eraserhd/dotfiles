{ lib, config, pkgs, options, ... }:

{
  config = {
    environment.systemPackages = with pkgs; [
      taskwarrior
    ];

    home-manager.users.jfelice = { pkgs, ... }: {
      programs.taskwarrior = {
        enable = true;
        dataLocation = "~/src/data/tasks";
        config = {
          news.version = "2.6.0";

          reserved.lines = "2";
          confirmation = "off";
          recurrence.confirmation = "off";

          uda.reviewed.type = "date";
          uda.reviewed.label = "Reviewed";
          report.review.description = "Weekly review";
          report.review.columns = "id,project,description.combined";
          report.review.sort = "project+";
          report.review.filter = "( reviewed.none: or reviewed.before:now-6days ) and ( +PENDING or +WAITING ) and recur.none: and -next";

          # Github
          uda.github.type = "string";
          uda.github.label = "GitHub ID";

          # Next
          report.next.columns = "id,start.age,entry.age,depends,priority,project,recur,scheduled.countdown,due.relative,until.remaining,description.truncated,urgency";
          report.next.labels = "ID,Active,Age,Deps,P,Project,Recur,S,Due,Until,Description,Urg";

          # Urgency
          urgency.annotations.coefficient = "0";
          urgency.project.coefficient = "0";
          urgency.active.coefficient = "25.0";
          urgency.due.coefficient = "20.0";

          # Colors
          rule.precedence.color = "deleted,completed,active,keyword.,tag.,project.,overdue,scheduled,due.today,due,blocked,blocking,recurring,tagged,uda.";

          # General decoration
          "color.label" = "";
          "color.label.sort" = "";
          color.alternate = "white on gray3";
          color.header = "yellow";
          color.footnote = "yellow";
          color.warning = "bold red";
          color.error = "white on red";
          color.debug = "blue";

          # Task state
          color.completed = "";
          color.deleted = "";
          color.active = "black on yellow";
          color.recurring = "";
          color.scheduled = "white on green";
          color.until = "";
          color.blocked = "black on color54";
          color.blocking = "black on color52";

          # Project
          color.project.none = "";

          # Priority
          color.uda.priority.H = "bold white";
          color.uda.priority.M = "white";
          color.uda.priority.L = "";

          # Tags
          color.tag.next = "bold yellow";
          color.tag.none = "";
          color.tagged = "green";

          # Due
          "color.due" = "";
          "color.due.today" = "red";
          "color.overdue" = "bold red";

          # UDA
          #color.uda.X=

          # Report: burndown
          color.burndown.done = "on green";
          color.burndown.pending = "on red";
          color.burndown.started = "on yellow";

          # Report: history
          color.history.add = "black on red";
          color.history.delete = "black on yellow";
          color.history.done = "black on green";

          # Report: summary
          color.summary.background = "white on black";
          color.summary.bar = "black on green";

          # Command: calendar
          "color.calendar.due" = "white on red";
          "color.calendar.due.today" = "bold white on red";
          "color.calendar.holiday" = "black on bright yellow";
          "color.calendar.overdue" = "black on bright red";
          "color.calendar.today" = "bold white on bright blue";
          "color.calendar.weekend" = "white on bright black";
          "color.calendar.weeknumber" = "bold blue";

          # Command: sync
          "color.sync.added" = "green";
          "color.sync.changed" = "yellow";
          "color.sync.rejected" = "red";

          # Command: undo
          "color.undo.after" = "green";
          "color.undo.before" = "red";
        };
      };
    };
  };
}
