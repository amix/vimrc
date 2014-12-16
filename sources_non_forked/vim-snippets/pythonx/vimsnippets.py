"""Helper methods used in UltiSnips snippets."""

def complete(tab, opts):
    """
    get options that start with tab

    :param tab: query string
    :param opts: list that needs to be completed

    :return: a string that start with tab
    """
    msg = "({0})"
    if tab:
        opts = [m[len(tab):] for m in opts if m.startswith(tab)]
    if len(opts) == 1:
        return opts[0]

    if not len(opts):
        msg = "{0}"
    return msg.format("|".join(opts))
