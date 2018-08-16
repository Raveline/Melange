# Melange

## Status

[![Build status](https://travis-ci.org/Raveline/Melange.svg?branch=master)](https://travis-ci.org/Raveline/Melange)

## Purpose

Melange is a blogging platform loosely inspired by Aby Warbur's Mnemosyne Atlas.
The purpose is mostly to blog without using one own's words, but rather quotes
or images by others.

It doesn't use static website generation, but rather the old-fashioned database
stored information. It uses no fancy javascript but good old HTML and CSS and a
fairly standard way of serving HTML pages.

The goal is to create "boards" containing several quotes and / or images all
connected to a same topic, thereby cleverly (or so one can hope) commenting
current affairs or the landscape on whatever topic one is interested in.

## Technical stack

The backend will use [Servant](https://haskell-servant.github.io/) for the API
management and [Squeal](https://github.com/morphismtech/squeal). Having a real
persistence layer is probably a bit overkill; a static site generation would fit
the goal, but this gives me a pretext to play with Squeal.

The frontend is mostly static HTML rendered through Servant. The administration
part of the website will however need some javascript - I'm working on a small
Vue.js app (I won't stream myself coding this though).
