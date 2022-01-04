import sys
import subprocess
import argparse
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
    subprocess.run(["git", "add", "runMain"])
    subprocess.run(["git", "commit", "-m", msg])

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description="Mako")

    parser.add_argument("--git-tag", dest="is_git_tag", action="store_true",
                        default=False, help="If set, will create Git tag. default: false")

    parser.add_argument("--git-commit", dest="is_git_commit", action="store_true",
                        default=False, help="If set, will Git commit. default: false")

    parser.add_argument("--render", dest="is_render", action="store_true",
                        default=False, help="If set, will render Mako templates. default: false")

    parser.add_argument("--version", dest="version", 
                        help="Janitor version")

    parser.add_argument("--msg", dest="msg", 
                        help="Git message. Must be set if --git is set")

    args = parser.parse_args()

    if args.version == None:
        print("Version must be set")
        sys.exit() 

    if args.msg == None:
        if args.is_git_tag == True or args.is_git_commit == True:
            print("Message --msg must be set if --git-tag or --git-commit is set")
            sys.exit() 

    if args.is_git_tag == False and args.is_git_commit == True:
        print("If --git-commit is set then --git-tag must be set")
        sys.exit() 

    print (args)

    if args.is_render == True:
        render_all(args.version)

    if args.is_git_commit == True:
        git_add(args.msg)

    if args.is_git_tag == True:
        add_git_tag(args.version,args.msg)



