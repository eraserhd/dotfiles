from kittens.tui.handler import result_handler
import subprocess
import json
import time

def main(args):
    pass

@result_handler(no_ui=True)
def handle_result(args, answer, target_window_id, boss):
    w = boss.window_id_map.get(target_window_id)
    if w is None:
        return ""
    a = time.time()
    os_window_id = w.child.environ.get("WINDOWID")
    if os_window_id is None:
        return ""
    b = time.time()
    text = subprocess.check_output(["yabai", "-m", "query", "--windows", "--window", str(os_window_id)], shell = False)
    c = time.time()
    data = json.loads(text)
    return str({'a': a, 'b': b, 'c': c, 'data': data["frame"]})
