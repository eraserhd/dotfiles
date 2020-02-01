{ pkgs, ... }:

{
  config = {
    environment.systemPackages = with pkgs; [
      tasksh
      taskwarrior
    ];

    home-manager.users.jfelice = { pkgs, ... }: {
      home.file.".taskrc".text = ''
        data.location=~/src/data/tasks
        reserved.lines=2

        # tasksh Review
        uda.reviewed.type=date
        uda.reviewed.label=Reviewed
        report._reviewed.description=Tasksh review report. Adjust the filter to your needs.
        report._reviewed.columns=uuid
        report._reviewed.sort=reviewed+,modified+
        report._reviewed.filter=( reviewed.none: or reviewed.before:now-6days ) and ( +PENDING or +WAITING )

        report.review.description=Weekly review
        report.review.columns=id,project,description.combined
        report.review.sort=project+
        report.review.filter=( reviewed.none: or reviewed.before:now-6days ) and ( +PENDING or +WAITING ) and recur.none:

        # Urgency
        urgency.annotations.coefficient=0
        urgency.project.coefficient=0

        # Colors
        rule.precedence.color=deleted,completed,active,keyword.,tag.,project.,overdue,scheduled,due.today,due,blocked,blocking,recurring,tagged,uda.

        # General decoration
        color.label=
        color.label.sort=
        color.alternate=white on gray3
        color.header=yellow
        color.footnote=yellow
        color.warning=bold red
        color.error=white on red
        color.debug=blue

        # Task state
        color.completed=
        color.deleted=
        color.active=black on yellow
        color.recurring=
        color.scheduled=white on green
        color.until=
        color.blocked=black on color54
        color.blocking=black on color52

        # Project
        color.project.none=

        # Priority
        color.uda.priority.H=bold white
        color.uda.priority.M=white
        color.uda.priority.L=

        # Tags
        color.tag.next=bold yellow
        color.tag.none=
        color.tagged=green

        # Due
        color.due=
        color.due.today=red
        color.overdue=bold red

        # UDA
        #color.uda.X=

        # Report: burndown
        color.burndown.done=on green
        color.burndown.pending=on red
        color.burndown.started=on yellow

        # Report: history
        color.history.add=black on red
        color.history.delete=black on yellow
        color.history.done=black on green

        # Report: summary
        color.summary.background=white on black
        color.summary.bar=black on green

        # Command: calendar
        color.calendar.due=white on red
        color.calendar.due.today=bold white on red
        color.calendar.holiday=black on bright yellow
        color.calendar.overdue=black on bright red
        color.calendar.today=bold white on bright blue
        color.calendar.weekend=white on bright black
        color.calendar.weeknumber=bold blue

        # Command: sync
        color.sync.added=green
        color.sync.changed=yellow
        color.sync.rejected=red

        # Command: undo
        color.undo.after=green
        color.undo.before=red
      '';

    };
  };
}
