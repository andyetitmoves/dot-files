#define PURPLE_PLUGINS

#include <gtk/gtk.h>

#include "version.h"

#include "gtkplugin.h"
#include "account.h"
#include "prpl.h"

#include "gtkblist.h"

#define _(MsgId) (MsgId)
#define N_(MsgId) (MsgId)

static void copy_status (PurpleBuddy *b)
{
	PurplePlugin *prpl;

	prpl = purple_find_prpl(purple_account_get_protocol_id(b->account));
	if (prpl != NULL) {
		PurplePluginProtocolInfo *prpl_info
			= PURPLE_PLUGIN_PROTOCOL_INFO(prpl);
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

static void plugin_action (PurplePluginAction* action)
{
	PidginBuddyList *blist = pidgin_blist_get_default_gtk_blist();
	PurpleBlistNode *node= blist->selected_node;
	if (node && (PURPLE_BLIST_NODE_IS_CONTACT(node) ||
		     PURPLE_BLIST_NODE_IS_BUDDY(node))) {
		PurpleBuddy *buddy;
		if (PURPLE_BLIST_NODE_IS_CONTACT(node)) {
			buddy = purple_contact_get_priority_buddy
				((PurpleContact*)node);
			if (! buddy)
				return;
		}
		else
			buddy = (PurpleBuddy*) node;
		copy_status (buddy);
	}
}

static GList* get_plugin_actions (PurplePlugin *plugin, gpointer context)
{
	GList *list = NULL;

	list = g_list_append
		(list, purple_plugin_action_new (_("Copy"), plugin_action));
	return list;
}

static PurplePluginInfo info = {
	PURPLE_PLUGIN_MAGIC,
	PURPLE_MAJOR_VERSION,
	PURPLE_MINOR_VERSION,
	PURPLE_PLUGIN_STANDARD,
	PIDGIN_PLUGIN_TYPE,
	0,
	NULL,
	PURPLE_PRIORITY_DEFAULT,
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

static void init_plugin(PurplePlugin *plugin) {
}

PURPLE_INIT_PLUGIN(copy_status, init_plugin, info);
