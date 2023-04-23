# Preface

## About Lisp

Lisp was originally born as a notation for computational models, and it was later discovered that it could be implemented and used as programming language, giving the language a unique origin.
Its history is quite old, with more than 60 years having passed since the idea was first proposed by John McCarthy.
Despite its old origins, Lisp remains an active and widely-used language, with various dialects continuing to be created and utilized on a wide range of platforms.
This can be attributed to the fact that Lisp's basic principles are very compact, making implementation easy, and that the language is easily extensible.
As a result, Lisp has been used for experimenting with the latest language features and as an embedded language in applications.

Among the Lisp dialects, Clojure and Common Lisp stand out as those that are currently being used for actual product development.
Clojure is a JVM language, allowing for interoperability with existing Java libraries and running on a wide range of platforms.
On the other hand, Common Lisp has been standardized by ANSI (American National Standards Institute), and programs written in accordance with the ANSI standard have portability and are guaranteed to work in the future.
For these reasons, Common Lisp has been used in fields such as industry, military, space development, and AI research.

Among the authors of this book are members of Pocket Change, a company developing an electronic money platform. Here, too, Common Lisp is being used for payment systems and backend web applications.

## Purpose and Target Audience of This Book

This book consists of explanations of recent development flows and tools in Common Lisp, as well as specific project examples. While there are a fair number of books on the language itself, there has been a lack of concrete guidance on how to set up an environment and create projects. This book aims to cover that gap, leaving detailed explanations of the language itself to other publications.

The target audience for this book includes those who are interested in Common Lisp and have researched or can research the language specifications but are unsure how to proceed with project development, as well as those who are considering entering the world of Common Lisp. Additionally, it is expected to be useful for those who are already developing with Common Lisp and want to learn about recent tools.

--------------------
## What is clfreaks?

clfreaks is a group focused on Common Lisp, centered around Eitaro Fukamachi and Masatoshi Sano. They organize hackathons and release [Podcasts](http://clfreaks.org) irregularly.

## Recommended Resources

### Existing Common Lisp Books

As mentioned earlier, this book is not intended to explain the language features of Common Lisp itself, so here are some recommended reference books. Please refer to these books along with this one.

- **Practical Common Lisp**: A comprehensive language guide. It contains many examples and is suitable as a first book.
  - The original English version is available for free at the following URL ([http://www.gigamonkeys.com/book/](http://www.gigamonkeys.com/book/)).
- **Paradigms of AI Programming: Case Studies in Common Lisp**: Commonly known as PAIP. You can learn Common Lisp through classical AI development.
  - The original English version is available for free at the following URL ([https://github.com/norvig/paip-lisp](https://github.com/norvig/paip-lisp)).
- **Land of Lisp**: You can learn Common Lisp through text-based game development and other examples.
- **On Lisp**: A book focused on explaining Common Lisp macros. It also contains many examples.
  - The original English version is available for free at the following URL ([http://www.paulgraham.com/onlisptext.html](http://www.paulgraham.com/onlisptext.html)).

### HyperSpec

[HyperSpec](http://www.lispworks.com/documentation/HyperSpec/Front/) is the ANSI Common Lisp specification provided by LispWorks. Although the page design is somewhat dated, it is an incredibly valuable resource available for free on the web. It can also be read from a text browser, allowing you to reference it within your editor.

When you are unsure about Common Lisp specifications, it is convenient to search with "clhs" (Common Lisp HyperSpec) on Google, like "clhs restart-case," and the desired page will appear.

### How to Find Libraries

There are two websites, Quickdocs and CLiki, for researching Common Lisp libraries by purpose.

- [Quickdocs](http://quickdocs.org) is a documentation site for Common Lisp libraries, organized by purpose.
- [CLiki](https://www.cliki.net) is a Wiki for Common Lisp operated by the Common Lisp Foundation.
On the "Current recommended libraries" page, you can find recommended libraries organized by purpose.
