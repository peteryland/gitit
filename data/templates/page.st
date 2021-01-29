<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
  <head>
    <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <meta name="description" content="$pagetitle$ - $wikititle$">
    <meta name="apple-mobile-web-app-capable" content="yes">
    <meta name="apple-mobile-web-app-status-bar-style" content="black">
    <link rel="preload" as="fetch" href="/_contents" />
    <link rel="preload" as="font" href="/fonts/MagnoliaIcons.woff" />
    <link rel="icon" type="image/png" href="/img/icons/mgnl-icon-72.png" sizes="72x72">
    <link rel="apple-touch-icon" href="/img/icons/mgnl-icon.png">
    <link rel="apple-touch-icon" sizes="57x57" href="/img/icons/mgnl-icon-57.png">
    <link rel="apple-touch-icon" sizes="72x72" href="/img/icons/mgnl-icon-72.png">
    <link rel="apple-touch-icon" sizes="114x114" href="/img/icons/mgnl-icon-114.png">
    <link rel="apple-touch-icon" sizes="144x144" href="/img/icons/mgnl-icon-144.png">
    <title>$pagetitle$ - $wikititle$</title>
    <link href="$base$/css/custom.css" rel="stylesheet" media="screen, projection" type="text/css" />
    <link href="$base$/css/micons.css" rel="stylesheet" media="screen, projection" type="text/css" />
    <link href="$base$/css/print.css" rel="stylesheet" media="print" type= "text/css" />
    <!--[if IE]><link href="$base$/css/ie.css" rel="stylesheet" media="screen, projection" type="text/css" /><![endif]-->
    <script>
      var sidebarmoved = 0;
      var oldsidebarwidth = "0";
      var sidebar;
      var sidebarmain;
      var pagetitle;
      var content;

      function onscrollhandler() {
        if (!content) content = document.getElementById("tl-content");
        if (!pagetitle) pagetitle = document.querySelector(".tl-pagetitle");
        if (content.scrollTop > 50) {
          pagetitle.style.transform = 'rotateX(90deg)';
          pagetitle.style.lineHeight = "0%";
          pagetitle.style.margin = 0;
        }
      }

      function onloadhandler() {
        var here = "$pagename$".split(' ').join('+');
        if (here.startsWith("_")) // top-level page like /_search
          here = ""
        var tlcontents = document.getElementById("tl-contents");
        fetch("/_contents", {cache: "force-cache"})
          .then(r => r.json())
          .then(contents => mkul(tlcontents, "/" + here, contents, ""));
      }

      document.addEventListener("DOMContentLoaded", onloadhandler);

      function sidebarhandler(e) {
        window.addEventListener('mousemove', doresize);
        window.addEventListener('mouseup', stopresize);
        sidebarmoved = 0;
        if (!sidebar) sidebar = document.getElementById("tl-sidebar");
        if (!sidebarmain) sidebarmain = document.getElementById("tl-sidebar-main");
        sidebar.style.transition = '';
      }
      function doresize(e) {
        sidebar.style.width = (e.clientX - sidebar.offsetLeft) + 'px';
        sidebarmain.style.transform = '';
        sidebarmoved = 1;
      }
      function stopresize(e) {
        sidebar.style.transition = 'width 0.2s';
        if (sidebarmoved === 0) {
          if (sidebar.clientWidth > 20) {
            oldsidebarwidth = sidebar.style.width;
            sidebar.style.minWidth = 0;
            sidebarmain.style.transform = 'rotateY(90deg)';
            sidebar.style.width = "0";
          } else {
            sidebar.style.minWidth = '';
            sidebarmain.style.transform = '';
            sidebar.style.width = oldsidebarwidth;
            oldsidebarwidth = "0";
          }
        }
        window.removeEventListener('mousemove', doresize);
        window.removeEventListener('mouseup', stopresize);
      }

      function mkul(parent, here, items, linkname) {
        if (items.length === 0) return;
        var ul = document.createElement("ul");
        for(var i = 0; i < items.length; i++) {
          var item = items[i];
          var li = document.createElement("li");
          li.appendChild(document.createElement("div"));
          var a = document.createElement("a");
          var newlinkname = linkname + "/" + item.t.split(' ').join('+');
          a.setAttribute("href", newlinkname);
          if (here === newlinkname) {
            li.setAttribute("class", "nav-here nav-open");
          } else {
            if (here.startsWith(newlinkname))
              li.setAttribute("class", "nav-parent nav-open");
          }
          a.appendChild(document.createTextNode(item.t));
          li.appendChild(a);
          ul.appendChild(li);
          mkul(li, here, item.c, newlinkname);
        }
        parent.appendChild(ul);
        parent.classList.add("nav-haschildren");
        parent.firstChild.addEventListener("click", function (e) { if (this === e.target) this.parentElement.classList.toggle("nav-open"); });
      }
    </script>
  </head>
  <body>
    <div id="tl-header">
      <p class="tl-crumbs">$crumblinks$</p>
      <h1 class="tl-pagetitle">$pagetitle$</h1>
    </div>
    <div id="tl-doc">
      <div id="tl-sidebar">
        <div id="tl-sidebar-main">
          <form action="$base$/_search" method="get">
            <input type="text" name="q" id="tl-q" placeholder="Search"/>
          </form>
          <ul>
            <li id="tl-contents" class="nav-parent nav-open"><div></div><a href="/">Magnolia documentation</a></li>
          </ul>
          <!-- ul style="margin: 0; padding: 0; list-style: none;">
            <li style="list-style: none"><a href="/_index" style="color: #CCC;">All Pages</a></li>
            <li style="list-style: none"><a href="/_random" style="color: #CCC;">Random Page</a></li>
          </ul -->
        </div>
        <div id="tl-sidebar-handle" onmousedown="sidebarhandler()"></div>
      </div>
      <div id="tl-main">
        <div id="tl-content" onscroll="onscrollhandler()">
          $content$
        </div>
        <div id="tl-footer">&copy; Copyright 2020 Magnolia</div>
      </div>
    </div>
  </body>
</html>
