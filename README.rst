`RESTAS`_ is a Common Lisp web application framework. Its key features
are:

* `RESTAS`_ was developed to simplify development of web applications
  following the `REST`_ architectural style.

* RESTAS is based on the `Hunchentoot`_ HTTP server. Web application
  development with RESTAS is in many ways simpler than with
  `Hunchentoot`_, but some knowledge of `Hunchentoot`_ is required, at
  least about working with hunchentoot:*request* and
  hunchentoot:*reply*.

* Request dispatch is based on a route system. The route system is the
  key concept of `RESTAS`_ and provides unique features not found in
  other web frameworks.

* The other key `RESTAS`_ concept is its module system, which provides
  a simple and flexible mechanism for modularized code reuse.

* Interactive development support. Any RESTAS code (such as the
  definition of a route, a module or a submodule) can be recompiled at
  any time when you work in `SLIME`_ and any changes you made can be
  immediately seen in the browser. No web server restart or other
  complicated actions are needed.

* `SLIME`_ integration. The inner structure of a web application can
  be investigated with the standard "SLIME Inspector." For example,
  there is a "site map" and a simple code navigation with this map.

* Easy to use, pure Lisp web application daemonization facility based on
  `RESTAS`_ and `SBCL`_ in Linux without the use of `Screen`_ or `detachtty`_.

* RESTAS is not an MVC framework, although it is not incompatible with the concept.
  From the MVC point of view, `RESTAS`_ provides the controller level. Nevertheless,
  `RESTAS`_ provides an effective and flexible way for separation of logic and
  representation, because it does not put any constraints on the structure of
  applications. Separation of model and controller can be effectively
  performed with Common Lisp facilities, and, hence, doesn't need any special
  support from the framework.

* RESTAS does not come with a templating
  library. `cl-closure-template`_ and `HTML-TEMPLATE`_ are two good
  templating libraries that can be used with RESTAS.

`RESTAS`_ is distributed under the terms of the `Lisp LGPL`_ license.

.. _RESTAS: http://restas.lisper.ru/
.. _SLIME: http://common-lisp.net/project/slime/
.. _Hunchentoot: http://www.weitz.de/hunchentoot/
.. _SBCL: http://www.sbcl.org/
.. _Screen: http://www.gnu.org/software/screen/
.. _detachtty: http://www.cliki.net/detachtty
.. _HTML-TEMPLATE: http://www.weitz.de/html-template/
.. _cl-closure-template: http://code.google.com/p/cl-closure-template/
.. _REST: http://en.wikipedia.org/wiki/Representational_State_Transfer
.. _Lisp LGPL: http://opensource.franz.com/preamble.html

