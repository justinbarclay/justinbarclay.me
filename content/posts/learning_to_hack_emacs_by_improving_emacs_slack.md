+++
title = "Learning to hack Emacs by improving emacs-slack.el"
author = ["Justin Barclay"]
date = 2018-03-24T00:00:00-06:00
lastmod = 2019-02-03T15:48:22-07:00
tags = ["emacs", "slack"]
categories = ["emacs"]
draft = false
weight = 2001
+++

## Where we left off {#where-we-left-off}

In my previous [post](/posts/learning_to_hack_emacs_by_sending_text_to_slack/) I figured out how to send any selected region in Emacs to Slack, wrapped in a markdown style code block (\`\`\`).

```emacs-lisp
(defun slack-select-rooms ()
  (interactive)
  (let* ((team (slack-team-select))
         (room (slack-room-select
                (cl-loop for team in (list team)
                         append (with-slots (groups ims channels) team
                                  (append ims groups channels))))))
    (slack-room-display room team)))
```

Unfortunately, that isn't enough for me! I don't always want to demarcate text as code, sometimes I just want to send naked text.

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

Or sometimes I want to have text don some shiny new quotes!

{{< figure src="/ox-hugo/garments.png" caption="Figure 1: Here we see some text being dressed in the freshest quotes of the season" >}}

```emacs-lisp
(defun jb/send-region-to-slack-quotes ()
  (interactive)
  (let* ((team (slack-team-select))  ;; Get all rooms from selected team
         (room (slack-room-select
                (cl-loop for team in (list team)
                         append (with-slots (groups ims channels) team
                                  (append ims groups channels))))))
    (slack-message-send-internal (concat "> "
                                         (filter-buffer-substring
                                          (region-beginning)
                                          (region-end)))
                                 (oref room id)
                                 team)))
```

{{< figure src="/ox-hugo/slack-quote-fail.gif" >}}

Sadly,  my simple solutions doesn't work. In [Markdown](https://daringfireball.net/projects/markdown/syntax#blockquote) you have to put ">" at the beginning of each line you want to specify as a quote.


## Houston We Have a Problem {#houston-we-have-a-problem}

I've run into a problem, a big problem: I don't know how to iterate through text in a buffer. Sure I can copy it --- copying is easy --- but editing text while not also editing the buffer is tricky.

Iterating through a list of strings is also easy. After all this is lisp, a language meant for list processing. If I want to edit a list of strings all I have to do is [map](https://www.gnu.org/software/emacs/manual/html%5Fnode/eintr/mapcar.html) over them and concat ">" to the beginning of each.

```emacs-lisp
(setq quotes '("I hope he didn’t die. Unless he left a note naming me his successor, then I hope he did die."
               "I’m so embarrassed. I wish everybody else was dead."
               "Have you ever tried simply turning off your TV, sitting down with your child, and hitting them?"))

(mapcar (lambda (line) (concat "> " line)) quotes)
```

> I hope he didn't die. Unless he left a note naming me his successor, then I hope he did die.
>
> I'm so embarrassed. I wish everybody else was dead.
>
> Blackmail is such an ugly word. I prefer extortion. The 'x' makes it sound cool.

If I step this up a notch and apply it to a region, I get an error letting me know that I am not doing what I think I am doing.

```emacs-lisp
(defun quote-region ()
  (interactive)
  (message
   (mapcar (lambda (line) (concat "> " line))
           (filter-buffer-substring (region-beginning) (region-end)))))
```

> Wrong type argument: sequencep, 40

As a beginner in elisp, I find interactive and programmatic text processing to be an oddity. I have built up an intuition for string manipulation in other environments. I would expect, text to be either a single string or an array of strings. However, that doesn't map well to the way Emacs operates on text in buffers.

My next guess is to try to split the buffer on newlines...

```emacs-lisp
(defun quote-region ()
  (interactive)
  (message
   (mapcar (lambda (line) (concat "> " line))
           (split-string
            (filter-buffer-substring (region-beginning) (region-end))
            "\n"
            t))))
```

> Wrong type argument: stringp, (#("> Hello" 2 7 (fontified t font-lock-fontified t help-echo nil src-block t ws-butler-chg chg ...)) #("> World" 2 7 (fontified t font-lock-fontified t help-echo nil src-block t ws-butler-chg chg ...)))

Surprisingly, this got me a lot farther, but now I'm hitting a type error somewhere. My first guess is that the message function is causing problems, which I can confirm by looking at the function signature of message, (message FORMAT-STRING &rest ARGS). Now, all I need to do is join this list of strings into one string and all of my woes will be solved.

Voila, we have a function that operates on a region by adding a quote marker to the beginning of each line, returning a string for use elsewhere.

```emacs-lisp
(defun quote-region ()
  (interactive)
  (message
   (string-join
    (mapcar (lambda (line) (concat "> " line))
            (split-string
             (filter-buffer-substring (region-beginning) (region-end))
             "\n"
             t))
    "\n")))
```

Now I have a function that works, but it's hacky --- way too hacky for me. I feel that text manipulation, _especially_ in a text editor, has to be easier than introducing the concept of a line, editing some text, and then removing the concept of the line. I believe that Emacs --- as a text editor --- has built methods for this and I have yet to discover them.

Unfortunately, I find the [documentation ](https://www.gnu.org/software/emacs/manual/html%5Fnode/elisp/Current-Buffer.html)[in ](https://www.gnu.org/software/emacs/manual/html%5Fnode/elisp/Excursions.html#Excursions)[Emacs](https://www.gnu.org/software/emacs/manual/html%5Fnode/elisp/Text-Lines.html#Text-Lines) is not really geared toward building up a mental framework for programmatically manipulating text. I had to do a [lot](http://ergoemacs.org/emacs/elisp%5Fprocess%5Flines.html) of [googling](https://emacs.stackexchange.com/a/2193) to get pointed in the right direction.

I realize that I am in a unique spot, though. Most of Emacs' text manipulation is meant to be in-place. But I want to:

1.  Copy a region/buffer
2.  Mutate some text
3.  Provide this text as a return value from a function
4.  Not mutate or change the current buffer

Emacs has all the tools to do this, and some of these tools are just easier to find than others.

After a lot of reading I've settled on a process. I'm going to:

1.  Copy the current region into a temporary buffer
2.  Loop over each line until we hit the end
3.  At the beginning of each line insert “> “
4.  Return the contents of this buffer[^fn:1]

```emacs-lisp
(defun jb/slack-quote-region (region)
  (with-temp-buffer
    (insert region)
    (goto-char 1) ;; Go to beginning of temporary buffer
    (while (> (point-max) (point)) ;; point is where cursor is in buffer, point-max is last position in buffer
      (beginning-of-line) ;; Always make sure we're at the beginning of the line
      (insert "> ") ;; Insert at point
      (forward-line 1)) ;; Go to next line
    (buffer-string))) ;; Return contents of temp buffer
```

This looks a lot more like idiomatic Emacs! To finish off this leg of my journey, I just need to add it to jb/send-region-to-slack-quotes.

```emacs-lisp
(defun jb/send-region-to-slack-quotes ()
  (interactive)
  (let* ((team (slack-team-select))  ;; Get all rooms from selected team
         (room (slack-room-select
                (cl-loop for team in (list team)
                         append (with-slots (groups ims channels) team
                                  (append ims groups channels))))))
    (slack-message-send-internal (jb/slack-quote-region
                                  (filter-buffer-substring
                                   (region-beginning)
                                   (region-end)))
                                 (oref room id)
                                 team)))
```

> In the beginning, the Universe was created. This has made a lot of people very angry and been widely regarded as a bad move.
>
> -- Douglas Adams

I'm not happy with the code that I've written so far. I mean, yeah it works, but it's ugly and repetitive. It's all very wet-behind-the-ears code --- I think with a bit of forethought and a big enough towel, I can dry it up.

Instead of having to call a different function for each decoration that I want to apply to my selected region, I should be able to delegate this work to one function and let the user decide what decoration they want. This is the perfect time to take advantage of [Emacs' completion framework](https://www.gnu.org/software/emacs/manual/html%5Fnode/elisp/Minibuffer-Completion.html).

Here's how the completing-read function works. It takes in a prompt and a list of choices. It then gives the list of choices to the user and then returns the user's response to the calling function.

```emacs-lisp
(setq choices '("It's amazing" "It's awesome" "Better than Vim"))
(completing-read "What do you think of Emacs?: " choices)
```

I've decided to take this a step further. I'm going to use an [alist](https://www.gnu.org/software/emacs/manual/html%5Fnode/elisp/Association-Lists.html) as a key-value store. The alist will be composed of short text describing the decoration they want to apply and a lambda function that applies the transform to the region. I am taking advantage of the fact that when completing-read is passed an association list, it takes the [car](https://www.gnu.org/software/emacs/manual/html%5Fnode/eintr/car-%5F0026-cdr.html) of each item in the list, and then presents those as the options for the user. Then, I can use assoc to find the first entry in our alist that matches the choice made by the user, and finally, have the chosen function operate on our selected region of text.

```emacs-lisp
(setq decorators '(("None" . (lambda (text) text)) ;; The identity function
                   ("Code"  . (lambda (text) (concat "```" text "```")))
                   ("Quote"  . (lambda (text) (jb/slack-quote-region text)))))

(defun decorate-text ()
  (interactive)
  (let ((decoration (completing-read "Select decoration: "
                                     decorators
                                     nil)
                                     t)
        (message (funcall (cdr (assoc decoration decorators)) "Oh yeah")))))
```


## Solution {#solution}

There we go---after digging through source code and reading through alot of Emacs documentation---I finally have a way to easily share snippets of code with friends, family, and cowokers. My present to you dear reader, for following me along on me jouney I give you my lifes work:

```emacs-lisp
(defun jb/slack-quote-region ()
    (with-temp-buffer
      (insert region)
      (goto-char 1)
      (while (> (point-max) (point))
        (beginning-of-line)
        (insert "> ")
        (forward-line 1))
      (buffer-string)))

(defun jb/decorate-text (text)
  (let* ((decorators '(("None" . (lambda (text) text))
                       ("Code"  . (lambda (text) (concat "```" text "```")))
                       ("Quote"  . (lambda (text) (jb/slack-quote-region text)))))
         (decoration (completing-read "Select decoration: "
                                      decorators
                                      nil
                                      t)))
    (funcall (cdr (assoc decoration decorators)) text)))

(defun jb/send-region-to-slack ()
  (interactive)
  (let* ((team (slack-team-select))
         (room (slack-room-select
                (cl-loop for team in (list team)
                         append (with-slots (groups ims channels) team
                                  (append ims groups channels))))))
    (slack-message-send-internal (jb/decorate-text (filter-buffer-substring
                                                    (region-beginning) (region-end)))
                                 (oref room id)
                                 team)))
```

![](/ox-hugo/perfet-opt.gif)
Nothing is more beautiful than code working as intended. Well, maybe my children? No, you're right, code is definitely more beautiful than my children.

I want to thank [@spiralganglion](http://twitter.com/spiralganglion) for being a tremendous friend and editor.


### Editor's note: {#editor-s-note}

These are transferred over from my medium blog, if you found any errors caused during the transition please let me know on twitter

[^fn:1]: I think it's important to note that all operations happened based around the [point](https://www.gnu.org/software/emacs/manual/html%5Fnode/eintr/Point-and-mark.html) and that the point follows along with the end of the text being inserted. So, when I add in text that is 5 characters long at the beginning of a line, the point's position moves from 0 to 4. This is why at the beginning of each loop we move point to the beginning of the line.
