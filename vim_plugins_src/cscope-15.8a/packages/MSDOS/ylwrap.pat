--- src/ylwrap	Tue Jun 26 15:23:52 2001
+++ src.DOS/ylwrap	Thu Jun 28 17:39:31 2001
@@ -80,7 +80,7 @@
     ;;
  *)
     # Make a symbolic link, hard link or hardcopy.
-    ln -s ../"$input" . > /dev/null 2>&1 || ln ../"$input" . > /dev/null 2>&1 || cp ../"$input" .
+    ln ../"$input" . > /dev/null 2>&1 || cp ../"$input" .
     ;;
 esac
 $prog ${1+"$@"} "$input"
