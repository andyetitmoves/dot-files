#define GAIM_PLUGINS

#include "version.h"

#include <gtk/gtk.h>

#include "gtkplugin.h"
#include "account.h"
#include "prpl.h"

#include "gtkblist.h"

#define _(MsgId) (MsgId)
#define N_(MsgId) (MsgId)

static void copy_status (GaimBuddy *b)
{
	GaimPlugin *prpl;

	prpl = gaim_find_prpl(gaim_account_get_protocol_id(b->account));
	if (prpl != NULL) {
		GaimPluginProtocolInfo *prpl_info
			= GAIM_PLUGIN_PROTOCOL_INFO(prpl);
		if (prpl_info && prpl_info->status_text) {
			GtkClipboard* clipbd = gtk_clipboard_get
				(gdk_atom_intern ("CLIPBOARD", FALSE));
			char *text = prpl_info->status_text(b);
			if (! text) text = g_strdup ("");
			gtk_clipboard_set_text (clipbd, text, -1);
			g_free (text);
		}
	}
}

static void plugin_action (GaimPluginAction* action)
{
	GaimGtkBuddyList *blist = gaim_gtk_blist_get_default_gtk_blist();
	GaimBlistNode *node= blist->selected_node;
	if (node && (GAIM_BLIST_NODE_IS_CONTACT(node) ||
		     GAIM_BLIST_NODE_IS_BUDDY(node))) {
		GaimBuddy *buddy;
		if (GAIM_BLIST_NODE_IS_CONTACT(node)) {
			buddy = gaim_contact_get_priority_buddy
				((GaimContact*)node);
			if (! buddy)
				return;
		}
		else
			buddy = (GaimBuddy*) node;
		copy_status (buddy);
	}
}

static GList* get_plugin_actions (GaimPlugin *plugin, gpointer context)
{
	GList *list = NULL;

	list = g_list_append
		(list, gaim_plugin_action_new (_("Copy"), plugin_action));
	return list;
}

static GaimPluginInfo info = {
	GAIM_PLUGIN_MAGIC,
	GAIM_MAJOR_VERSION,
	GAIM_MINOR_VERSION,
	GAIM_PLUGIN_STANDARD,
	GAIM_GTK_PLUGIN_TYPE,
	0,
	NULL,
	GAIM_PRIORITY_DEFAULT,
	"gtk-andyetitmoves-copystatus",
	_("Copy Status"),
	"1.0",
	_("Copy the status of the selected buddy to clipboard"),
	_("Adds a plugin action to \
copy the status of the selected buddy to clipboard"),
	"R.Ramkumar <andyetitmoves@gmail.com>",
	"http://www.cs.iitm.ernet.in/~ramk",
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	get_plugin_actions
};

static void init_plugin(GaimPlugin *plugin) {
}

GAIM_INIT_PLUGIN(copy_status, init_plugin, info);
