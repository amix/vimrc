<?php
if ($_FILES["file"]["error"] > 0)
  {
    echo "Error: " . $_FILES["file"]["error"] . "<br />";
  }
else
  {
    echo "Size: " . ($_FILES["file"]["size"] / 1024) . " Kb<br />";
  }
?>
<html>
hello (world)
</html>
