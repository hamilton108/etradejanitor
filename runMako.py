import sys
import subprocess
from mako.template import Template

TPL_DIR="python/templates"

def render_tpl(janitorVersion,tpl_file,target):
    tpl_file_x = "%s/%s" % (TPL_DIR,tpl_file)
    tpl = Template(filename=tpl_file_x)
    result = tpl.render(janitorVersion=janitorVersion)
    # print(result)
    f = open(target,'w')
    f.write(result)
    f.close()

def render_all(janitorVersion):
    render_tpl(janitorVersion,"docker/dockerbuild.mako","docker/dockerbuild")
    render_tpl(janitorVersion,"docker/Dockerfile.mako","docker/Dockerfile")
    render_tpl(janitorVersion,"docker/main.sh.mako","docker/main.sh")
    render_tpl(janitorVersion,"runMain.mako","./runMain")

def add_git_tag(janitorVersion,msg):
    jv = "v%s" % janitorVersion
    subprocess.run(["git", "tag", "-a", jv, "-m", msg])

def git_add(msg):
    subprocess.run(["git", "add", "docker/dockerbuild"])
    subprocess.run(["git", "add", "docker/Dockerfile"])
    subprocess.run(["git", "add", "docker/main.sh"])
    subprocess.run(["git", "add", "runMain.sh"])
    subprocess.run(["git", "commit", "-m", msg])

if __name__ == '__main__':
    janitorVersion = sys.argv[1]
    if (len(sys.argv) >= 3):
        gitmsg = sys.argv[2]
    else:
        gitmsg = None

    render_all(janitorVersion)

    if gitmsg != None: 
        git_add(msg)
        add_git_tag(janitorVersion,gitmsg)


