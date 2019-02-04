+++
title = "Hacking Emacs to send text to Slack"
author = ["Justin Barclay"]
date = 2018-03-03T00:00:00-07:00
lastmod = 2019-02-03T15:43:06-07:00
tags = ["emacs", "slack"]
categories = ["emacs"]
draft = false
weight = 2001
+++

## Introduction {#introduction}

I want to get Emacs to send snippets of text from any buffer to Slack. Luckily, there is a Slack client for Emacs called [emacs-slack](https://github.com/yuya373/emacs-slack). So, most of the hard work is done for me --- I have an interface to Slack in Emacs that allows me to send and read messages in any channel, room, or direct message. Unfortunately, I am now at an uncanny valley. I think emacs-slack does a lot of things right, but I don't think it makes sharing as easy as it should be.


### Why? {#why}

Why do I use Emacs? Mostly because I like to break things, and it is very easy for me to break my Emacs set-up.

_No, why..._

Why do I use Slack inside of Emacs? I like my Slack like I like my Emacs: brittle and glued together with arcane magic I don't understand. There is also the side benefit that Slack in Emacs takes up fewer resources than the Slack desktop app.

_No! Why isn't emacs-slack good enough?_

Oh, well, because I am a developer and I like to share things, and I am lazy --- too lazy to copy and paste every time I want to share an idea or have a question about a line of code. So instead of copying and pasting, I plan on spending way too long reading someone else's code, figuring out how to make sharing code with my team easier.

You, uh, don't wanna go for a ride do you?

{{< figure src="/ox-hugo/carpet-ride.png" >}}


### TL;DR {#tl-dr}

Install [emacs-slack](https://github.com/yuya373/emacs-slack) and paste the code below into your init file to be able to send a snippet of code to Slack.

```emacs-lisp
(defun jb/send-region-to-slack-code ()
  (interactive)
  (let ((team (slack-team-select)) ;; Select team
        (room (slack-room-select
               (cl-loop for team in (list team)
                        for channels = (oref team channels)
                        nconc channels)))) ;; Get all rooms from selected team
    (slack-message-send-internal (concat "```"(filter-buffer-substring (region-beginning) (region-end)) "```")
                                 (oref room id)
                                 team)))
```


## Let's play! {#let-s-play}

If you wish to follow along, you will need to install and set up [emacs-slack.](https://github.com/yuya373/emacs-slack)


### Install {#install}

If you use [use-package](https://github.com/jwiegley/use-package) as your Emacs package manager, [Yuya373](https://github.com/yuya373) has some code in the emacs-slack repo that you can just [copy and paste](https://github.com/yuya373/emacs-slack#configure) into your init file.

If you don't use use-package, you could use [borg](https://github.com/emacscollective/borg) to assimilate the package into your Emacs.

If you don't use borg, you could use [el-get](https://github.com/dimitri/el-get) to install the packages for you.

If you don't have any of these, a nice simple _M-x package-install emacs-slack_ works wonders.

If you don't want to do any of this, you could give up on the good life and use vim.


### Setup {#setup}

To set up emacs-slack, just follow the helpful steps found on the [GitHub](https://github.com/yuya373/emacs-slack#how-to-get-token-the-harder-yet-officially-sanctioned-way) page.

If you've never heard of emacs-slack before, this will probably be enough for you. You have emacs-slack setup and running. Play with it. Live it. Love it. Forget about this post.


## Still Here? Let's Dive In {#still-here-let-s-dive-in}

All I need to do is figure out which functions in emacs-slack are responsible for sending messages, how to hook into those functions, and how I can programmatically access text in Emacs. Simple? Simple.


### slack.el {#slack-dot-el}

My first steps will be to open up some of emacs-slack's files and see what I can divine. [slack.el](https://github.com/yuya373/emacs-slack/blob/master/slack.el) seems like a good place to start. Let's see, there are a couple of [defcustoms](https://www.gnu.org/software/emacs/manual/html%5Fnode/eintr/defcustom.html), some defuns, and cl-defuns[^fn:1]... but nothing that looks like it has to do with sending messages, which is ultimately what I want to do.

Time to move on...


### slack-message.el {#slack-message-dot-el}

I want to send a message, maybe [slack-message.el](https://github.com/yuya373/emacs-slack/blob/master/slack-message.el) is a better place to start.

Ok... wait, what? defclass? defmethod? Since when was Emacs Lisp object oriented[^fn:2]?

So, it looks like Message is an object with a lot of methods. Helpful, but doesn't get me anything yet.

I've read through two files, I've learned some things about the structure of this package, but there are 50 .el files. If I keep looking through all of these files, it's going to be a lot of work! Maybe I can take a different approach.


### Through the power of the profiler, I shall not be defeated! {#through-the-power-of-the-profiler-i-shall-not-be-defeated}

I know of two ways to learn how code works in Emacs, besides just reading the source code. The first is the profiler built right into [Emacs](https://www.gnu.org/software/emacs/manual/html%5Fnode/elisp/Profiling.html), and the other is the Elisp debugger, [Edebug](https://www.gnu.org/software/emacs/manual/html%5Fnode/elisp/Edebug.html#Edebug). For now, I'm going to start with the profiler --- the easier approach.

_M-x profiler-start_ will track CPU, memory, or a combination of the two. My first thought is to send a few messages in Slack and see what pops up in the profiler. Hopefully, that will point me in the right direction and I'll find the magical invocations I need to recite to get my idea to work.


### A Peek Behind The Veil {#a-peek-behind-the-veil}

Let's look at the top level of the profile trace found in the CPU buffer. Only two calls look like they are worth investigating: `command-execute` and `…`.


#### CPU {#cpu}

```text
+ command-execute                                             67%
+ redisplay_internal (C function)                             21%
+ lui-scroll-post-command                                      9%
+ #<compiled 0x4da9630d>                                       0%
+ emojify-update-visible-emojis-background-after-command       0%
+ company-post-command                                         0%
+ request--curl-callback                                       0%
+ ...                                                          0%
+ timer-event-handler                                          0%
+ undo-auto--add-boundary                                      0%
+ sp--save-pre-command-state                                   0%
+ global-hl-line-highlight                                     0%
```


### Command-execute {#command-execute}

Expanding command-execute, we start to see some interesting calls...


#### Enhance {#enhance}

```text
- command-execute                                         67%
 - call-interactively                                     67%
  - apply                                                 67%
   - call-interactively@ido-cr+-record-current-command    63%
    - apply                                               63%
     - #<subr call-interactively>                         63%
      - funcall-interactively                             63%
       + profiler-report                                  63%
       - lui-send-input                                    0%
        - slack-message--send                              0%
         - let*                                            0%
          - if                                             0%
           - let*                                          0%
            - if                                           0%
             - slack-buffer-send-message                   0%
              - apply                                      0%
               - #<compiled 0x4f23dd71>                    0%
                - apply                                    0%
                 - #<compiled 0x4db1669d>                  0%
                  - apply                                  0%
                   - #<lambda 0xdafed4764d8>               0%
                    - let*                                 0%
                     - slack-message-send-internal         0%
                      - let*                               0%
                       - let*                              0%
                        + slack-ws-send                    0%
                        + json-encode                      0%
                        + list                             0%
                        + slack-message-create             0%
```

_slack-message --- send_ and _slack-message-send-internal_ seem to be the most promising, so let's look at these.

```emacs-lisp
(defun slack-message--send (message)
  (slack-if-let* ((buf slack-current-buffer))
      (slack-if-let* ((command (slack-slash-commands-parse message)))
          (slack-buffer-execute-slash-command buf command)
        (slack-buffer-send-message buf message))))
```

It looks like _slack-message --- send_ checks to see if the current buffer is a "Slack Buffer", looks for "Slack Commands" to execute in the buffer, then passes the message onto another function _slack-buffer-send-message_. Unfortunately, this seems to rely too much on the internal state of the package, so let's move on to the next function and hope it's simpler.

```emacs-lisp
(defun slack-message-send-internal (message channel-id team)
  (slack-message-inc-id team)
  (with-slots (message-id sent-message self-id) team
    (let* ((m (list :id message-id
                    :channel channel-id
                    :type "message"
                    :user self-id
                    :text (slack-message-prepare-links
                           (slack-escape-message message)
                           team)))
           (json (json-encode m))
           (obj (slack-message-create m team)))
      (slack-ws-send json team)
      (puthash message-id obj sent-message))))
```

Next up on my list is _slack-message-send-internal_. This immediately looks a lot more promising. It takes exactly the data I would expect: a message, a room id, and a team. Then, it composes the data into a keyed list and sends a JSON encoded object through a WebSocket. Jackpot!

Now onto my next problem...


### Bugging Out {#bugging-out}

The Emacs profiler is nice to see what is being called, but how do I see what the data structures look like? I mean I need to know what they look like to insert them in the send-message-send-internal, right? [Edebug](https://www.gnu.org/software/emacs/manual/html%5Fnode/elisp/Edebug.html#Edebug) to the rescue! If you have any intention of writing elisp, I recommend you read this section of the Emacs Manual. I've only recently discovered Edebug, but it has quickly become an invaluable tool when I explore code.


#### Tracing Through slack-message--send {#tracing-through-slack-message-send}

I know what function I want to inspect, _slack-message-send-internal_, but I'm also curious: how is data transformed and built-up as it's moving through this system? To answer that question we need to start inspecting earlier in the call chain. We've already taken a quick look at slack-send--message, so let's add a [source breakpoint](https://www.gnu.org/software/emacs/manual/html%5Fnode/elisp/Source-Breakpoints.html#Source-Breakpoints), and [instrument](https://www.gnu.org/software/emacs/manual/html%5Fnode/elisp/Instrumenting.html#Instrumenting) the function.

```emacs-lisp
(defun slack-message--send (message)
  (edebug)
  (slack-if-let* ((buf slack-current-buffer))
      (slack-if-let* ((command (slack-slash-commands-parse message)))
          (slack-buffer-execute-slash-command buf command)
        (slack-buffer-send-message buf message))))
```

I've added a breakpoint to the function, _edebug_. Now we just need to instrument the function. An easy way to instrument functions is to move my cursor to the beginning of the function definition and call _M-x edebug-eval-top-level-form_. This evaluates the current function and instruments it so Edebug can perform its magic.

After tracing through the functions I see that message, channel-id, and team have the following structure:

| message    | #("Hello World" 0 4 (fontified t ws-butler-chg chg)) |
|------------|------------------------------------------------------|
| channel-id | "D884GPDM0"                                          |
| team       | #23=#<slack-team slack-team-454a4604>                |

It looks like message can be any string. I still need to find out how to select the team and channel I want to post to.


### slack-channel-select {#slack-channel-select}

Luckily, I have a good idea of where to look. Every time I want to enter a Slack channel I run the command _M-x slack-channel-select_, so let's take a look at that.

```emacs-lisp
(defun slack-channel-select ()
  (interactive)
  (let* ((team (slack-team-select))
         (room (slack-room-select
                (cl-loop for team in (list team)
                         for channels = (oref team channels)
                         nconc channels))))
    (slack-room-display room team)))
```

That looks perfect. I can copy-paste 90% of this code into my own function and we'll have something close to working.


### So A Foo Walks Into A Bar {#so-a-foo-walks-into-a-bar}

My first test was to see if I could quickly modify this function to get a prototype working.

```emacs-lisp
(defun jb/say-hello-to-slack ()
  (interactive)
  (let* ((team (slack-team-select))
         (room (slack-room-select
                (cl-loop for team in (list team)
                         for channels = (oref team channels)
                         nconc channels))))
    (slack-message-send-internal "Hello World"
                                 (oref room id)
                                 team)))
```

Now to test it!

{{< figure src="/ox-hugo/hello-world.gif" caption="Figure 1: Example of the above function jb/say-hello-to-slack" >}}


### Buffers, Regions, And Everything Between {#buffers-regions-and-everything-between}

Now the last problem I need to solve: I need to figure out how to copy a region of text. I'm not sure how to do that, but I do know of a great resource for learning elisp, Emacs' own [Elisp Manual](https://www.gnu.org/software/emacs/manual/html%5Fnode/elisp/)[^fn:4]. The key parts that we need to be aware of from this manual are [Regions](https://www.gnu.org/software/emacs/manual/html%5Fnode/elisp/The-Region.html#The-Region) and [Buffer Contents.](https://www.gnu.org/software/emacs/manual/html%5Fnode/elisp/Buffer-Contents.html#Buffer-Contents)

As an example of how I learned to programmatically access text in a region, I've outlined a simple function below that prints out the content of a selected region to the minibuffer.

```emacs-lisp
(defun jb/echo-region ()
  (interactive)
  (message (filter-buffer-substring (region-beginning) (region-end))))
```

This finally leads me to have all the tools to create a function where I can post from any buffer into slack

```emacs-lisp
(defun jb/send-region-to-slack ()
  (interactive)
  (let ((team (slack-team-select)) ;; Select team
        (room (slack-room-select
               (cl-loop for team in (list team)
                        for channels = (oref team channels)
                        nconc channels)))) ;; Get all rooms from selected team
    (slack-message-send-internal (filter-buffer-substring (region-beginning) (region-end))
                                 (oref room id)
                                 team)))
```


#### Codifying My Message {#codifying-my-message}

One last enhancement to my function that I want to make: I am almost always going to be sending some chunk of code to Slack, so I want to wrap it in three backticks so Slack will apply the proper markup to it.

```emacs-lisp
(defun jb/send-region-to-slack-code ()
  (interactive)
  (let ((team (slack-team-select)) ;; Select team
        (room (slack-room-select
               (cl-loop for team in (list team)
                        for channels = (oref team channels)
                        nconc channels)))) ;; Get all rooms from selected team
    (slack-message-send-internal (concat "```"(filter-buffer-substring (region-beginning) (region-end)) "```")
                                 (oref room id)
                                 team)))
```

1700 words to describe a 10 line function, I don't understand all the hate that Emacs gets.

-    Editor's note:

    These are transferred over from my medium blog, if you found any errors caused during the transition please let me know on twitter

[^fn:1]: As I was reading through the slack code I found it interesting that Emacs' defun was different from a CL implementation of defun. Richard Stallman hated how you could use keys to destructure arguments in Common Lisp and chose to omit that feature in elisp. <https://www.emacswiki.org/emacs/KeywordArguments>
[^fn:2]: Fun note, Emacs Lisp has had an object system, ”[Enhanced Implementation of Emacs Interpreted Objects](https://www.gnu.org/software/emacs/manual/html%5Fnode/eieio/)”, since at least 2007 and maybe earlier[^fn:3].
[^fn:3]: EIEIO is actually inspired by [Common Lisp Object System](https://en.wikipedia.org/wiki/Common%5FLisp%5FObject%5FSystem), doing this dive into Emacs-Slack is teaching me so much about Emacs and Common Lisp!
[^fn:4]: This is a lie, I didn't know about this manual until I started writing this post. This would have saved me hours of very poor google-fu.
