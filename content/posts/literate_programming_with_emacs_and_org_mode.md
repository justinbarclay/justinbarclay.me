+++
title = "Literate Programming with org-mode and restclient"
author = ["Justin Barclay"]
date = 2019-02-02T00:00:00-07:00
lastmod = 2019-02-02T01:06:33-07:00
tags = ["hugo", "org"]
categories = ["emacs", "rest", "org"]
draft = true
weight = 2001
+++

## Literate Programming with org-mode and restclient {#literate-programming-with-org-mode-and-restclient}


### Rest Call Me Maybe {#rest-call-me-maybe}

> TL;DR Other HTTP Clients aren’t that great. Here we use Emacs and restclient, with public APIs, to identify plants and share on Twitter. Emacs and restclient offer a great user experience and workflow when documenting and exploring APIs.

Lately, I've spent a lot of time exploring web APIs. I've tried a couple of tools over the years: Postman back when it was a chrome extension, Curl, and HTTPie. They were all, ok — they got the job done — but they all ended up missing some feature or some UX. That's when I found [restclient](https://github.com/pashky/restclient.el), a package for Emacs with a simple DSL, and could convert the restclient request to an equivalent curl request.

Let's take a look at an example of what restclient looks like when authenticating with twitter. Here we will post our base64 encoded secrets to get a bearer token:

```plaintext
POST https://api.twitter.com/oauth2/token
Authorization: Basic QmRUTklkSEI5WWZHbVhwSkZ6NTZManpUNjpBMk1rOHJjaVdEWFdva3FCN1pmemZFdEk3WjRNd1lpM3JFSjhzN1JoVm9xMXhZY2pMbQ==
Content-Type: application/x-www-form-urlencoded;charset=UTF-8

grant_type=client_credentials
```

Then once, you've POSTed the request a new buffer opens up with the response.
\#+NAME restclient response

```highlighttext
{
  "token_type": "bearer",
  "access_token": "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA%2FAAAAAAAAAAAAAAAAAAAA%3DAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
}
// POST https://api.twitter.com/oauth2/token?grant_type=client_credentials
// HTTP/1.1 200 OK
// cache-control: no-cache, no-store, must-revalidate, pre-check=0, post-check=0
// content-disposition: attachment; filename=json.json
// content-length: 155
// content-type: application/json;charset=utf-8
// date: Thu, 24 Jan 2019 05:54:09 GMT
// expires: Tue, 31 Mar 1981 05:00:00 GMT
// last-modified: Thu, 24 Jan 2019 05:54:09 GMT
// ml: S
// pragma: no-cache
// server: tsa_a
// status: 200 OK
// strict-transport-security: max-age=631138519
// x-connection-hash: b93d89db0ee8f4ea90991c99c8d58449
// x-content-type-options: nosniff
// x-frame-options: DENY
// x-response-time: 20
// x-transaction: 005403e60014e9ee
// x-twitter-response-tags: BouncerCompliant
// x-ua-compatible: IE=edge,chrome=1
// x-xss-protection: 1; mode=block; report=https://twitter.com/i/xss_report
// Request duration: 0.079710s
```

Using [restclient](https://www.youtube.com/watch?v=fTvQTMOGJaw) as my primary means to play with APIs worked for me for quite a while. It was a easy-to-read document, that housed all the requests in a single file, and lived alongside the rest of my code in git.

However, I eventually ran into three complications when using it:

1.  My restclient files would become a mess and navigating to find a specific endpoint or a set of endpoints was a pain.
2.  `I am lazy and dealing with authenticated APIs was a pain.`
    -   I'd need to enter my credentials each time I needed to get a new token
    -   Once I authenticated, I would have to copy and paste that token from the response buffer into a variable, to use throughout the restclient file.
3.  For exploring APIs I like to take notes, record responses, and record the input.

There was a simple solution for my first and last points, which was to use [org-mode](https://www.youtube.com/watch?v=GK3fij-D1G8). The power and flexibility of Emacs, and its suite of packages, constantly amazes me. I've been a long-time fan of org-mode. In my opinion it is a great replacement for Markdown and a number of other document formats in Emacs. I've found that org-mode is a great place to practice [literate programming](https://www.offerzen.com/blog/literate-programming-empower-your-writing-with-emacs-org-mode), but it's also a great place to an interactive style of programming, like a REPL or a Jupyter Notebook. Org-mode has a [babel](https://orgmode.org/worg/org-contrib/babel/) extension for [restclient](https://github.com/alf/ob-restclient.el). Babel is an extension that runs code contained in SRC block: `#+BEGIN_SRC...#+END_SRC` and saves anything sent to STDOUT or value returned, and then can be used in other SRC code blocks.

If you've never heard of org-mode, here's three key things to know before going forward:

1.  Here is a [short video](https://www.youtube.com/watch?v=lsYdK0C2RvQ) exploring some of its features and here is a much [longer video](https://www.youtube.com/watch?v=GK3fij-D1G8) on literate programming in Emacs
2.  This entire document was written using org-mode and babel
3.  You can execute this document yourself, given you change your credentials ;)

{{< figure src="/ox-hugo/org-mode-low-quality.gif" caption="Figure 1: Tweeting using literate programming and Emacs" >}}


#### Follow along at home! {#follow-along-at-home}

If you would like run arbitrary elisp at home, you can download the following file in Emacs or your favourite [flavours](http://spacemacs.org/) of [Vim](https://github.com/hlissner/doom-emacs).

```lisp
(browse-url-emacs "https://raw.githubusercontent.com/justinbarclay/-_-/master/twitter_has_put_me_in_a_vegetative_state.org")
```

Of course, you need to go out and download restclient and ob-restclient and thats it, thats all the Emacs packages you need. Oh... also NodeJS and Ruby, don't @ me. Ok @ me, I'm lonely.


### Twitter {#twitter}

A good way to demonstrate the power of restclient and org-mode would be to post some tweets from Emacs. First, we need to be able to authenticate with Twitter. So, let’s see how we can use org-mode and restclient to authenticate with an OAuth endpoint.


#### Helper functions {#helper-functions}

First, we need to define a few functions that we are going to use during OAuth authentication. I could use a library or package for this, but when I am writing I become somewhat of a masochist.

```lisp
(defun twitter-signing-key (consumer-secret token-secret)
  "Creates a signing key by combining the consumer-secret and the
   token secret and percent encoding the result"
  (concat
   (url-encode-url
    consumer-secret)
   "&"
   (url-encode-url
    token-secret)))

(defun twitter-signature-string (method base params)
  "Builds a hex encoded string of the format METHOF&BASE&PARAM1=VALUE1..."
  (let ((sorted-params
         (sort params
               (lambda (first second)
                 (string< (car first) (car second))))))
    (concat
     method
     "&"
     (url-hexify-string base)
     "&"
     (url-hexify-string
      (mapconcat
       (lambda (entry)
         (let ((key (car entry))
               (value (cdr entry)))
           (concat (url-hexify-string key)
                   "="
                   (url-hexify-string value))))
       sorted-params
       "&")))))

(defun build-twitter-header-string (header oauth-headers)
  "Takes in a list of cons cells that represent HTTP headers, as well as
   the information needed to define the OAUTH response for a Twitter request,
   and build a restclient style header string"
  (concat
   "<<\n"
   (mapconcat
    (lambda (entry)
      (let ((key (car entry))
            (value (cdr entry)))
        (concat
         key
         ": "
         value
         " ")))
    header
    "")
   "\nAuthorization: OAuth "
   (string-trim-right
    (mapconcat
     (lambda (entry)
       (let ((key (car entry))
             (value (cdr entry)))
         (concat
          key
          "="
          "\"" value "\""
          ",")))
     oauth-headers
     " ")
    ",")
   "\n#"))
```


#### Shhh It's a Secret {#shhh-it-s-a-secret}

I don't need to store the authentication information in files, and I am to lazy to remember or copy and paste them! I can just use the information that is stored in my environment.

<a id="code-snippet--twitter-consumer-key"></a>
```bash
echo $TWITTER_CONSUMER_KEY
```

<a id="code-snippet--twitter-consumer-secret"></a>
```bash
echo $TWITTER_CONSUMER_SECRET
```

<a id="code-snippet--twitter-access-token"></a>
```bash
echo $TWITTER_ACCESS_TOKEN
```

<a id="code-snippet--twitter-access-secret"></a>
```bash
echo $TWITTER_ACCESS_SECRET
```


#### Let's Auth it {#let-s-auth-it}

-    Step 1: Generate a body

    Before we can do all the fun authentication bits that is OAuth, we need to have some content. So, I feel like I need to be on brand for an Emacs user and let everyone know I am using Emacs for something that isn't editing text.

    <a id="code-snippet--hello-world"></a>
    ```lisp
    (setq twitter-body (list (cons "status" "Hello world! I'm tweeting from Emacs")))
    ```

-    Step 2: Generating some header data

    Ok, now that we have our Twitter status, we need to auto generate a few more pieces of information; a nonce, a timestamp, and the signature.

    Emacs doesn't really have a built in crypto library, but do you know who does? Ruby! It is a fun language with a pretty full featured standard library; let's use it to generate our nonce.

    <a id="code-snippet--nonce"></a>
    ```ruby
    require 'securerandom'

    nonce = SecureRandom.uuid
    nonce.gsub(/\W/, "")
    ```

    Our request is going to need a time signature.

    <a id="code-snippet--oauth-time"></a>
    ```lisp
    (format-time-string "%s")
    ```

    We need to define the headers that we need for this request.

    <a id="code-snippet--twitter-headers"></a>
    ```lisp
    (list
     (cons "Content-Type" "application/x-www-form-urlencoded"))
    ```

    Did I mention Emacs built-in cryptography is kind of lacking? Well, we'll need to let another language do the heavy lifting when signing the request. I like Node and Node has a decent crypto library built into it. In the example below I am defining a code block as a function that I am going to call later and use it in an emacs-lisp source block.

    <a id="code-snippet--createSignature"></a>
    ```js
    let crypto = require('crypto')

    let createSignature = (key, text) => {
      return crypto.createHmac('sha1', key).update(signature_string).digest();
    }

    return createSignature(key, signature_string).toString('base64');
    ```

    Now before we can sign anything, and we **do** need to sign things, we need to create a signing key. We can use our consumer-secret and our access-secret to build a Twitter signing key.

    <a id="code-snippet--signing-key"></a>
    ```lisp
    (twitter-signing-key consumer-secret token-secret)
    ```

-    Step 3: Creating The Header

    Next up, we need to build the header, create a string to sign, sign that string, and then add that signature to our header. Simple.

    <a id="code-snippet--twitter-oauth-headers"></a>
    ```lisp
    (let*
        ((twitter-oauth-headers
          (list
           (cons "oauth_consumer_key" consumer-key)
           (cons "oauth_nonce" nonce)
           (cons "oauth_signature_method" "HMAC-SHA1")
           (cons "oauth_timestamp" oauth-time)
           (cons "oauth_token" access-token)
           (cons "oauth_version" "1.0")))
         (signature-string
          (twitter-signature-string "POST"
                                    "https://api.twitter.com/1.1/statuses/update.json"
                                    (append twitter-oauth-headers twitter-body)))
         (signature
          (org-sbe createSignature
                   (signature_string (eval signature-string))
                   (key (eval signing-key))))) ;; Here I am calling that signing function that I defined in NodeJS
      (append twitter-oauth-headers (list (cons "oauth_signature"
                                                (url-hexify-string signature)))))
    ```

-    Step 4: Encoding Data and Posting To Twitter

    Up next, our headers need to be in a string format that `restclient` knows how to read.

    <a id="code-snippet--twitter-restclient-headers"></a>
    ```lisp
    (build-twitter-header-string header (sort twitter-oauth-headers
                                              (lambda (first second)
                                                (string< (car first) (car second)))))
    ```

    We need to encode our body as a post parameter string.

    <a id="code-snippet--twitter-post-body"></a>
    ```lisp
    (setq twitter-post-body
          (concat
           ""
           (mapconcat
            (lambda (entry)
              (concat (car entry) "=" (url-hexify-string (cdr entry))))
            twitter-body
            "&")
           ""))
    ```

    Finally, now that we've done all that work to format and sign things, we can finish it off by tweeting to the world how much we love Emacs.

    ```hljs
    #
    :body := (concat twitter-post-body)
    POST https://api.twitter.com/1.1/statuses/update.json?:body
    :twitter-headers
    ```


### Taking things to 11 {#taking-things-to-11}

{{< figure src="/ox-hugo/to_11.gif" >}}

I think using org-mode and restclient to authenticate and post on Twitter is a little too mundane. Can we do anything more elaborate?

Why, of course we can! This is Emacs, we pretty much have to do something overly complicated.

I'm a big fan of science and I want to share my enthusiasm with the world. So, we're going to use our newly learned skills to talk across several APIs. We're going to:

1.  Grab a plant name from [Trefle](https://www.trefle.io)
2.  Find a picture of that plant, using [Google Custom Search](https://cse.google.com/cse/)
3.  Make sure that the picture we have is of that plant, using [Google Vision](https://cloud.google.com/vision/)
4.  Tag someone on Twitter and share the plant name and picture with them


#### More Helper Functions {#more-helper-functions}

We need a function to sanitize the response we get from restclient

```lisp
(defun sanitize-restclient-response (string)
 "Trim down a restclient response to JSON, removing the org source block and
  header information"
 (string-trim (replace-regexp-in-string
               "^#\\+BEGIN_SRC js\\|^#\\+END_SRC\\|^//[[:print:]]+"
               ""
               string)))
```

Let's be able to execute an arbitrary source code block

```elisp
(defun run-org-block (&optional code-block-name)
  (save-excursion
    (let ((code-block (or code-block-name
                          (completing-read "Code Block: " (org-babel-src-block-names))))
          (goto-char
           (org-babel-find-named-block
            code-block))
          (org-babel-execute-src-block-maybe)))))
```

Here's a couple of functions we're going to use to help us parse a response from Google's API.

```lisp
(defun parse-ml-response (responses)
  "Extracts a Google AI response down to a list of label annotations"
  (let* ((json-response (json-read-from-string responses))
         (label-annotations  (cdr
                             (assoc 'labelAnnotations
                                    (elt
                                     (cdr (assoc 'responses json-response))
                                     0)))))
    label-annotations))

(defun contains-description-p (annotations descriptions)
  "Checks to see if any of the items in the sequence ANNOTATIONS has a
   description that matches one of the items in DESCRIPTIONS"
  (let ((annotated-descriptions (mapcar
                                 (lambda (item)
                                   (cdr (assoc 'description item)))
                                 annotations)))
    (reduce (lambda (predicate description)
              (if predicate
                  predicate
                (if (member (downcase description) descriptions)
                    't
                  nil)))
            annotated-descriptions
            :initial-value nil)))
```


#### Harvesting a name {#harvesting-a-name}

Let's give our source block a name, `#+NAME: trefle`, so we can easily reference it throughout the rest of our notebook. I am using my Mac's keychain to store and retrieve an access token I have stored for trefle.io.

<a id="code-snippet--trefle"></a>
```bash
security find-generic-password -gws trefle.io
```

To import a variable from earlier in the file you can use `:var token=trefle` where :var token specifies that you want to insert a variable called token into the proceeding block and the contents of that variable are pulled from a source code block named `trefle`. Now we just need to build the HTTP headers we're going to use for our interaction with Trefle.

<a id="code-snippet--trefle-headers"></a>
```lisp
(concat
   "<<
Content-Type: application/json
Accept: application/json
Authorization: Bearer " token)
```

As of the last time I looked, [Trefle](https://www.trefle.io) has over 4000 pages of plants, so we want to get a random plant off of a random page. So to start, we'll generate a page number from 0 to 4000...

<a id="code-snippet--plants"></a>
```hljs
#
:page := (random 4000)
GET https://trefle.io/api/plants?page=:page
:headers
#
```

Before we can do anything with the output we need to clean it up. Restclient likes to have all the headers for the response at the bottom of the buffer, so we need to filter those out of the response.

<a id="code-snippet--sanitized-response"></a>
```lisp
(sanitize-restclient-response response)
```

Now we could use lisp, but everyone has Node installed and Node is pretty much built for parsing JSON, so it only makes sense to use that. We'll grab a random plant from the results and return its name.

<a id="code-snippet--plant-name"></a>
```js
let index = Math.floor(Math.random() * 30);
return JSON.parse(plants)[index].scientific_name;
```


#### Imagine {#imagine}

I need to get my Google API key. For this, I've been lazy and have just been storing it in my environment.

<a id="code-snippet--google-api-key"></a>
```bash
echo $GOOGLE_API_KEY
```

We've got a plant name, now we need an image of the plant.

<a id="code-snippet--google-images"></a>
```hljs
GET https://content.googleapis.com/customsearch/v1?cx=009341007550343915479%3Afg_hsgzltxw&q=:plant-name&searchType=image&key=:api-key
```

Much like our search for a plant name, we need to clean up the response from Google API so it's easily parsable as JSON.

<a id="code-snippet--flower-images"></a>
```lisp
(sanitize-restclient-response google-images)
```

We have a nice list of plant images. Let's play Google roulette and use the first image from the search.

<a id="code-snippet--plant-image"></a>
```js
return "" + JSON.parse(plant_images).items[0].link
```


#### What shall we (machine) learn today? {#what-shall-we--machine--learn-today}

When we're running our code we don't have time to make sure all it does what it is supposed to do and everyone knows you can't trust Google. Instead, we'll use machine learning provided by the fabulous Google Vision API to validate our choice. We'll ask Google for the top 3 labels for an image and see if those labels contain the words "Flower", "Plant", or "Tree".

<a id="code-snippet--plant-ml-results"></a>
```hljs
POST https://vision.googleapis.com/v1/images:annotate?key=:api-key
{
  "requests":[
    {
      "image":{
        "source":{
          "imageUri":
          :plant-image
        }
      },
      "features":[
        {
          "type":"LABEL_DETECTION",
          "maxResults":3
        }
      ]
    }
  ]
}
```

Next clean the response.

<a id="code-snippet--sanitized-ml-results"></a>
```lisp
(sanitize-restclient-response response)
```

If you're curious what talking to the Google Vision API looks like, here it is.

```text
{
  "responses": [
    {
      "labelAnnotations": [
        {
          "mid": "/m/04_tb",
          "description": "map",
          "score": 0.9684097,
          "topicality": 0.9684097
        },
        {
          "mid": "/m/03scnj",
          "description": "line",
          "score": 0.734654,
          "topicality": 0.734654
        },
        {
          "mid": "/m/07j7r",
          "description": "tree",
          "score": 0.7276011,
          "topicality": 0.7276011
        }
      ]
    }
  ]
}
```

Let's check to see if the first three descriptors come back as plant, tree, or a flower. If it doesn't match these descriptors, then we rerun them code block below. When we rerun ths code block `image-is-plant-p` this will have a cascading effect and call all previous code blocks. So, we'll keep querying Trefle and Google until we find a pciture that meets our criteria. Warning: this could trap us into an infinite loop.

<a id="code-snippet--image-is-plant-p"></a>
```lisp
(while (not (contains-description-p
             (parse-ml-response response)
             '("plant" "tree" "flower")))
    (org-sbe image-is-plant-p))
```


#### A rose by any other name {#a-rose-by-any-other-name}

We need one last piece of information before we can demonstrate our love of plants to the world: someone to tweet at. Let's ask ourselves for some input.

<a id="code-snippet--twitter-handle"></a>
```lisp
(read-string "What is the twitter handle of someone you want to tweet? ")
```


#### Content is king {#content-is-king}

Now we need to [build our body](https://www.youtube.com/watch?v=6BxAJNywkmg) into something we can process later...

<a id="code-snippet--twitter-plant-body"></a>
```lisp
(setq twitter-body
 (list
  (cons "status" (concat "" twitter_handle " " plant_name " " (replace-regexp-in-string "'" "" plant_image)))))
```


#### A real header scratcher: creating a new Twitter header {#a-real-header-scratcher-creating-a-new-twitter-header}

We can use all of the source blocks we created back when we were professing our love for Emacs. However, we need to change a few references. In the source block below we need to change the reference from `body=hello-world` to `body=twitter-plant-body`.

<a id="code-snippet--twitter-oauth-headers-plants"></a>
```lisp
(let*
    ((twitter-oauth-headers
      (list
       (cons "oauth_consumer_key" consumer-key)
       (cons "oauth_nonce" nonce)
       (cons "oauth_signature_method" "HMAC-SHA1")
       (cons "oauth_timestamp" oauth-time)
       (cons "oauth_token" access-token)
       (cons "oauth_version" "1.0")))
     (signature-string
      (twitter-signature-string "POST"
                                "https://api.twitter.com/1.1/statuses/update.json"
                                (append twitter-oauth-headers twitter-body)))
     (signature
      (org-sbe createSignature
               (signature_string (eval signature-string))
               (key (eval signing-key)))))
  (append twitter-oauth-headers (list (cons "oauth_signature"
                                            (url-hexify-string signature)))))
```


#### Repeat: Encoding Data and Posting To Twitter {#repeat-encoding-data-and-posting-to-twitter}

Similiarly, we need to reassign `twitter-oauth-headers=twitter-oauth-headers` to `twitter-oauth-headers=twitter-oauth-headers-plants`

<a id="code-snippet--twitter-restclient-headers-plants"></a>
```lisp
(build-twitter-header-string header (sort twitter-oauth-headers
                                          (lambda (first second)
                                            (string< (car first) (car second)))))
```

Again, we encode our body...

<a id="code-snippet--twitter-post-body-1"></a>
```lisp
(setq twitter-post-body
      (concat
       ""
       (mapconcat
        (lambda (entry)
          (concat (car entry) "=" (url-hexify-string (cdr entry))))
        twitter-body
        "&")
       ""))
```

Voila! We've can post a cute plant...or tree...or flower...to Twitter!

```hljs
#
:body := (concat twitter-post-body)
POST https://api.twitter.com/1.1/statuses/update.json?:body
:twitter-headers
```


### In conclusive {#in-conclusive}

You don't need to use Emacs to enjoy the benefits of literate programming across 4 disparate APIs. To do that all you really need is bubblegum and determination. However, literate programming is a great paradigm to create and share your work for others to play with. I think Emacs and in particular, org-mode, is great for literate programming because it's a lot like [Jupyter Notebooks](https://jupyter.org/), it supports a lot more languages and supports more than one language per notebook. Plus, org-mode works wonderfully with the two best [flavours](http://spacemacs.org/) of [Vim](https://github.com/hlissner/doom-emacs)!


## References {#references}

-   <https://developer.twitter.com/en/docs/basics/authentication/overview/application-only>
-   <https://cloud.google.com/vision/docs/request>
-   <https://developer.twitter.com/en/docs/tweets/post-and-engage/api-reference/post-statuses-update.html>
-   <https://developers.google.com/custom-search/docs/overview>
-   <http://lti.tools/oauth/>
