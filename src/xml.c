/* Interface to libxml2.
   Copyright (C) 2010-2015 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.  */

#include <config.h>

#ifdef HAVE_LIBXML2

#include <libxml/tree.h>
#include <libxml/parser.h>
#include <libxml/HTMLparser.h>

#include "lisp.h"
#include "buffer.h"


#ifdef WINDOWSNT

# include <windows.h>
# include "w32.h"

DEF_DLL_FN (htmlDocPtr, htmlReadMemory,
	     (const char *, int, const char *, const char *, int));
DEF_DLL_FN (xmlDocPtr, xmlReadMemory,
	     (const char *, int, const char *, const char *, int));
DEF_DLL_FN (xmlNodePtr, xmlDocGetRootElement, (xmlDocPtr));
DEF_DLL_FN (void, xmlFreeDoc, (xmlDocPtr));
DEF_DLL_FN (void, xmlCleanupParser, (void));
DEF_DLL_FN (void, xmlCheckVersion, (int));

static bool
libxml2_loaded_p (void)
{
  Lisp_Object found = Fassq (Qlibxml2_dll, Vlibrary_cache);

  return CONSP (found) && EQ (XCDR (found), Qt);
}

# undef htmlReadMemory
# undef xmlCheckVersion
# undef xmlCleanupParser
# undef xmlDocGetRootElement
# undef xmlFreeDoc
# undef xmlReadMemory

# define htmlReadMemory fn_htmlReadMemory
# define xmlCheckVersion fn_xmlCheckVersion
# define xmlCleanupParser fn_xmlCleanupParser
# define xmlDocGetRootElement fn_xmlDocGetRootElement
# define xmlFreeDoc fn_xmlFreeDoc
# define xmlReadMemory fn_xmlReadMemory

static bool
load_dll_functions (HMODULE library)
{
  LOAD_DLL_FN (library, htmlReadMemory);
  LOAD_DLL_FN (library, xmlReadMemory);
  LOAD_DLL_FN (library, xmlDocGetRootElement);
  LOAD_DLL_FN (library, xmlFreeDoc);
  LOAD_DLL_FN (library, xmlCleanupParser);
  LOAD_DLL_FN (library, xmlCheckVersion);
  return true;
}

#else  /* !WINDOWSNT */

static bool
libxml2_loaded_p (void)
{
  return true;
}

#endif	/* !WINDOWSNT */

static bool
init_libxml2_functions (void)
{
#ifdef WINDOWSNT
  if (libxml2_loaded_p ())
    return true;
  else
    {
      HMODULE library;

      if (!(library = w32_delayed_load (Qlibxml2_dll)))
	{
	  message1 ("libxml2 library not found");
	  return false;
	}

      if (! load_dll_functions (library))
	goto bad_library;

      Vlibrary_cache = Fcons (Fcons (Qlibxml2_dll, Qt), Vlibrary_cache);
      return true;
    }

 bad_library:
  Vlibrary_cache = Fcons (Fcons (Qlibxml2_dll, Qnil), Vlibrary_cache);

  return false;
#else  /* !WINDOWSNT */
  return true;
#endif	/* !WINDOWSNT */
}

static Lisp_Object
make_dom (xmlNode *node)
{
  if (node->type == XML_ELEMENT_NODE)
    {
      Lisp_Object result = list1 (intern ((char *) node->name));
      xmlNode *child;
      xmlAttr *property;
      Lisp_Object plist = Qnil;

      /* First add the attributes. */
      property = node->properties;
      while (property != NULL)
	{
	  if (property->children &&
	      property->children->content)
	    {
	      char *content = (char *) property->children->content;
	      plist = Fcons (Fcons (intern ((char *) property->name),
				    build_string (content)),
			     plist);
	    }
	  property = property->next;
	}
      result = Fcons (Fnreverse (plist), result);

      /* Then add the children of the node. */
      child = node->children;
      while (child != NULL)
	{
	  result = Fcons (make_dom (child), result);
	  child = child->next;
	}

      return Fnreverse (result);
    }
  else if (node->type == XML_TEXT_NODE || node->type == XML_CDATA_SECTION_NODE)
    {
      if (node->content)
	return build_string ((char *) node->content);
      else
	return Qnil;
    }
  else if (node->type == XML_COMMENT_NODE)
    {
      if (node->content)
	return list3 (intern ("comment"), Qnil,
		      build_string ((char *) node->content));
      else
	return Qnil;
    }
  else
    return Qnil;
}

static Lisp_Object map_error (int code);

static Lisp_Object
parse_region (Lisp_Object start, Lisp_Object end, Lisp_Object base_url,
	      Lisp_Object discard_comments, bool htmlp, bool signal_errors_p)
{
  xmlDoc *doc;
  Lisp_Object result = Qnil;
  const char *burl = "";
  ptrdiff_t istart, iend, istart_byte, iend_byte;

  xmlCheckVersion (LIBXML_VERSION);

  validate_region (&start, &end);

  istart = XINT (start);
  iend = XINT (end);
  istart_byte = CHAR_TO_BYTE (istart);
  iend_byte = CHAR_TO_BYTE (iend);

  if (istart < GPT && GPT < iend)
    move_gap_both (iend, iend_byte);

  if (! NILP (base_url))
    {
      CHECK_STRING (base_url);
      burl = SSDATA (base_url);
    }

  if (htmlp)
    doc = htmlReadMemory ((char *) BYTE_POS_ADDR (istart_byte),
			  iend_byte - istart_byte, burl, "utf-8",
			  HTML_PARSE_RECOVER|HTML_PARSE_NONET|
			  HTML_PARSE_NOWARNING|HTML_PARSE_NOERROR|
			  HTML_PARSE_NOBLANKS);
  else
    doc = xmlReadMemory ((char *) BYTE_POS_ADDR (istart_byte),
			 iend_byte - istart_byte, burl, "utf-8",
			 XML_PARSE_NONET|XML_PARSE_NOWARNING|
			 XML_PARSE_NOBLANKS |XML_PARSE_NOERROR);

  if (doc != NULL)
    {
      Lisp_Object r = Qnil;
      if (NILP(discard_comments))
        {
          /* If the document has toplevel comments, then this should
             get us the nodes and the comments. */
          xmlNode *n = doc->children;

          while (n) {
            if (!NILP (r))
              result = Fcons (r, result);
            r = make_dom (n);
            n = n->next;
          }
        }

      if (NILP (result)) {
	/* The document doesn't have toplevel comments or we discarded
	   them.  Get the tree the proper way. */
	xmlNode *node = xmlDocGetRootElement (doc);
	if (node != NULL)
	  result = make_dom (node);
      } else
	result = Fcons (Qtop, Fcons (Qnil, Fnreverse (Fcons (r, result))));

      xmlFreeDoc (doc);
    }
  else if (signal_errors_p)
    {
      xmlErrorPtr err;
      Lisp_Object error_message;
      ptrdiff_t msglen;

      err = xmlGetLastError ();
      msglen = strlen (err->message);
      /* Drop trailing newline, if present */
      if (msglen > 0 && err->message[msglen - 1] == '\n')
	error_message = make_string (err->message, msglen - 1);
      else
	error_message = make_string (err->message, msglen);

      xsignal2 (Qxml_error, error_message, map_error (err->code));
    }

  return result;
}

void
xml_cleanup_parser (void)
{
  if (libxml2_loaded_p ())
    xmlCleanupParser ();
}

DEFUN ("libxml-parse-html-region", Flibxml_parse_html_region,
       Slibxml_parse_html_region,
       2, 4, 0,
       doc: /* Parse the region as an HTML document and return the parse tree.
If BASE-URL is non-nil, it is used to expand relative URLs.
If DISCARD-COMMENTS is non-nil, all HTML comments are discarded. */)
  (Lisp_Object start, Lisp_Object end, Lisp_Object base_url, Lisp_Object discard_comments)
{
  if (init_libxml2_functions ())
    return parse_region (start, end, base_url, discard_comments, true, false);
  return Qnil;
}

DEFUN ("libxml-parse-xml-region", Flibxml_parse_xml_region,
       Slibxml_parse_xml_region,
       2, MANY, 0,
       doc: /* Parse the region as an XML document and return the parse tree.
If BASE-URL is non-nil, it is used to expand relative URLs.
If DISCARD-COMMENTS is non-nil, all HTML comments are discarded.
If SIGNAL-ERRORS is nil, any parse error will cause this function
to return nil.  Otherwise, errors will be signaled as `xml-error'.
usage: (libxml-parse-xml-region START END &optional BASE-URL DISCARD-COMMENTS &key SIGNAL-ERRORS) */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  Lisp_Object start = args[0];
  Lisp_Object end = args[1];
  Lisp_Object base_url = Qnil;
  Lisp_Object discard_comments = Qnil;
  Lisp_Object signal_errors = Qnil;
  ptrdiff_t n;

  if (nargs >= 3)
    base_url = args[2];

  if (nargs >= 4)
    discard_comments = args[3];

  for (n = 4; n < nargs; n++)
    {
      if (EQ (args[n], QCxml_signal_errors))
	{
	  n++;
	  if (n >= nargs)
	    xsignal2 (Qwrong_number_of_arguments,
		      intern ("libxml-parse-xml-region"),
		      make_number (nargs));
	  signal_errors = args[n];
	}
      else
	{
	  /* XXX: bad keyword argument. suitable error? */
	  xsignal2 (Qwrong_number_of_arguments,
		    intern ("libxml-parse-xml-region"),
		    make_number (nargs));
	}
    }

  if (init_libxml2_functions ())
    return parse_region (start, end, base_url, discard_comments, false,
			 EQ (Qnil, signal_errors) ? false : true);

  if (EQ (Qnil, signal_errors))
    return Qnil;
  else
    xsignal2 (Qxml_error, build_string ("Cannot initialize XML library"),
	      Qxml_error_cannot_init_library);
}


/***********************************************************************
			    Initialization
 ***********************************************************************/
void
syms_of_xml (void)
{
  defsubr (&Slibxml_parse_html_region);
  defsubr (&Slibxml_parse_xml_region);

  DEFSYM (QCxml_signal_errors, ":signal-errors");

  DEFSYM (Qxml_error, "xml-error");
  Fput (Qxml_error, Qerror_conditions,
	list2 (Qxml_error, Qerror));
  Fput (Qxml_error, Qerror_message,
	build_pure_c_string ("XML parse error"));

  DEFSYM (Qxml_error_cannot_init_library, "cannot-init-library");

  /* From the enum xmlParserErrors in xmlerror.h */

  DEFSYM (QCxml_error_err_ok, ":err-ok");
  DEFSYM (QCxml_error_err_internal_error, ":err-internal-error");
  DEFSYM (QCxml_error_err_no_memory, ":err-no-memory");
  DEFSYM (QCxml_error_err_document_start, ":err-document-start");
  DEFSYM (QCxml_error_err_document_empty, ":err-document-empty");
  DEFSYM (QCxml_error_err_document_end, ":err-document-end");
  DEFSYM (QCxml_error_err_invalid_hex_charref, ":err-invalid-hex-charref");
  DEFSYM (QCxml_error_err_invalid_dec_charref, ":err-invalid-dec-charref");
  DEFSYM (QCxml_error_err_invalid_charref, ":err-invalid-charref");
  DEFSYM (QCxml_error_err_invalid_char, ":err-invalid-char");
  DEFSYM (QCxml_error_err_charref_at_eof, ":err-charref-at-eof");
  DEFSYM (QCxml_error_err_charref_in_prolog, ":err-charref-in-prolog");
  DEFSYM (QCxml_error_err_charref_in_epilog, ":err-charref-in-epilog");
  DEFSYM (QCxml_error_err_charref_in_dtd, ":err-charref-in-dtd");
  DEFSYM (QCxml_error_err_entityref_at_eof, ":err-entityref-at-eof");
  DEFSYM (QCxml_error_err_entityref_in_prolog, ":err-entityref-in-prolog");
  DEFSYM (QCxml_error_err_entityref_in_epilog, ":err-entityref-in-epilog");
  DEFSYM (QCxml_error_err_entityref_in_dtd, ":err-entityref-in-dtd");
  DEFSYM (QCxml_error_err_peref_at_eof, ":err-peref-at-eof");
  DEFSYM (QCxml_error_err_peref_in_prolog, ":err-peref-in-prolog");
  DEFSYM (QCxml_error_err_peref_in_epilog, ":err-peref-in-epilog");
  DEFSYM (QCxml_error_err_peref_in_int_subset, ":err-peref-in-int-subset");
  DEFSYM (QCxml_error_err_entityref_no_name, ":err-entityref-no-name");
  DEFSYM (QCxml_error_err_entityref_semicol_missing, ":err-entityref-semicol-missing");
  DEFSYM (QCxml_error_err_peref_no_name, ":err-peref-no-name");
  DEFSYM (QCxml_error_err_peref_semicol_missing, ":err-peref-semicol-missing");
  DEFSYM (QCxml_error_err_undeclared_entity, ":err-undeclared-entity");
  DEFSYM (QCxml_error_war_undeclared_entity, ":war-undeclared-entity");
  DEFSYM (QCxml_error_err_unparsed_entity, ":err-unparsed-entity");
  DEFSYM (QCxml_error_err_entity_is_external, ":err-entity-is-external");
  DEFSYM (QCxml_error_err_entity_is_parameter, ":err-entity-is-parameter");
  DEFSYM (QCxml_error_err_unknown_encoding, ":err-unknown-encoding");
  DEFSYM (QCxml_error_err_unsupported_encoding, ":err-unsupported-encoding");
  DEFSYM (QCxml_error_err_string_not_started, ":err-string-not-started");
  DEFSYM (QCxml_error_err_string_not_closed, ":err-string-not-closed");
  DEFSYM (QCxml_error_err_ns_decl_error, ":err-ns-decl-error");
  DEFSYM (QCxml_error_err_entity_not_started, ":err-entity-not-started");
  DEFSYM (QCxml_error_err_entity_not_finished, ":err-entity-not-finished");
  DEFSYM (QCxml_error_err_lt_in_attribute, ":err-lt-in-attribute");
  DEFSYM (QCxml_error_err_attribute_not_started, ":err-attribute-not-started");
  DEFSYM (QCxml_error_err_attribute_not_finished, ":err-attribute-not-finished");
  DEFSYM (QCxml_error_err_attribute_without_value, ":err-attribute-without-value");
  DEFSYM (QCxml_error_err_attribute_redefined, ":err-attribute-redefined");
  DEFSYM (QCxml_error_err_literal_not_started, ":err-literal-not-started");
  DEFSYM (QCxml_error_err_literal_not_finished, ":err-literal-not-finished");
  DEFSYM (QCxml_error_err_comment_not_finished, ":err-comment-not-finished");
  DEFSYM (QCxml_error_err_pi_not_started, ":err-pi-not-started");
  DEFSYM (QCxml_error_err_pi_not_finished, ":err-pi-not-finished");
  DEFSYM (QCxml_error_err_notation_not_started, ":err-notation-not-started");
  DEFSYM (QCxml_error_err_notation_not_finished, ":err-notation-not-finished");
  DEFSYM (QCxml_error_err_attlist_not_started, ":err-attlist-not-started");
  DEFSYM (QCxml_error_err_attlist_not_finished, ":err-attlist-not-finished");
  DEFSYM (QCxml_error_err_mixed_not_started, ":err-mixed-not-started");
  DEFSYM (QCxml_error_err_mixed_not_finished, ":err-mixed-not-finished");
  DEFSYM (QCxml_error_err_elemcontent_not_started, ":err-elemcontent-not-started");
  DEFSYM (QCxml_error_err_elemcontent_not_finished, ":err-elemcontent-not-finished");
  DEFSYM (QCxml_error_err_xmldecl_not_started, ":err-xmldecl-not-started");
  DEFSYM (QCxml_error_err_xmldecl_not_finished, ":err-xmldecl-not-finished");
  DEFSYM (QCxml_error_err_condsec_not_started, ":err-condsec-not-started");
  DEFSYM (QCxml_error_err_condsec_not_finished, ":err-condsec-not-finished");
  DEFSYM (QCxml_error_err_ext_subset_not_finished, ":err-ext-subset-not-finished");
  DEFSYM (QCxml_error_err_doctype_not_finished, ":err-doctype-not-finished");
  DEFSYM (QCxml_error_err_misplaced_cdata_end, ":err-misplaced-cdata-end");
  DEFSYM (QCxml_error_err_cdata_not_finished, ":err-cdata-not-finished");
  DEFSYM (QCxml_error_err_reserved_xml_name, ":err-reserved-xml-name");
  DEFSYM (QCxml_error_err_space_required, ":err-space-required");
  DEFSYM (QCxml_error_err_separator_required, ":err-separator-required");
  DEFSYM (QCxml_error_err_nmtoken_required, ":err-nmtoken-required");
  DEFSYM (QCxml_error_err_name_required, ":err-name-required");
  DEFSYM (QCxml_error_err_pcdata_required, ":err-pcdata-required");
  DEFSYM (QCxml_error_err_uri_required, ":err-uri-required");
  DEFSYM (QCxml_error_err_pubid_required, ":err-pubid-required");
  DEFSYM (QCxml_error_err_lt_required, ":err-lt-required");
  DEFSYM (QCxml_error_err_gt_required, ":err-gt-required");
  DEFSYM (QCxml_error_err_ltslash_required, ":err-ltslash-required");
  DEFSYM (QCxml_error_err_equal_required, ":err-equal-required");
  DEFSYM (QCxml_error_err_tag_name_mismatch, ":err-tag-name-mismatch");
  DEFSYM (QCxml_error_err_tag_not_finished, ":err-tag-not-finished");
  DEFSYM (QCxml_error_err_standalone_value, ":err-standalone-value");
  DEFSYM (QCxml_error_err_encoding_name, ":err-encoding-name");
  DEFSYM (QCxml_error_err_hyphen_in_comment, ":err-hyphen-in-comment");
  DEFSYM (QCxml_error_err_invalid_encoding, ":err-invalid-encoding");
  DEFSYM (QCxml_error_err_ext_entity_standalone, ":err-ext-entity-standalone");
  DEFSYM (QCxml_error_err_condsec_invalid, ":err-condsec-invalid");
  DEFSYM (QCxml_error_err_value_required, ":err-value-required");
  DEFSYM (QCxml_error_err_not_well_balanced, ":err-not-well-balanced");
  DEFSYM (QCxml_error_err_extra_content, ":err-extra-content");
  DEFSYM (QCxml_error_err_entity_char_error, ":err-entity-char-error");
  DEFSYM (QCxml_error_err_entity_pe_internal, ":err-entity-pe-internal");
  DEFSYM (QCxml_error_err_entity_loop, ":err-entity-loop");
  DEFSYM (QCxml_error_err_entity_boundary, ":err-entity-boundary");
  DEFSYM (QCxml_error_err_invalid_uri, ":err-invalid-uri");
  DEFSYM (QCxml_error_err_uri_fragment, ":err-uri-fragment");
  DEFSYM (QCxml_error_war_catalog_pi, ":war-catalog-pi");
  DEFSYM (QCxml_error_err_no_dtd, ":err-no-dtd");
  DEFSYM (QCxml_error_err_condsec_invalid_keyword, ":err-condsec-invalid-keyword");
  DEFSYM (QCxml_error_err_version_missing, ":err-version-missing");
  DEFSYM (QCxml_error_war_unknown_version, ":war-unknown-version");
  DEFSYM (QCxml_error_war_lang_value, ":war-lang-value");
  DEFSYM (QCxml_error_war_ns_uri, ":war-ns-uri");
  DEFSYM (QCxml_error_war_ns_uri_relative, ":war-ns-uri-relative");
  DEFSYM (QCxml_error_err_missing_encoding, ":err-missing-encoding");
  DEFSYM (QCxml_error_war_space_value, ":war-space-value");
  DEFSYM (QCxml_error_err_not_standalone, ":err-not-standalone");
  DEFSYM (QCxml_error_err_entity_processing, ":err-entity-processing");
  DEFSYM (QCxml_error_err_notation_processing, ":err-notation-processing");
  DEFSYM (QCxml_error_war_ns_column, ":war-ns-column");
  DEFSYM (QCxml_error_war_entity_redefined, ":war-entity-redefined");
  DEFSYM (QCxml_error_err_unknown_version, ":err-unknown-version");
  DEFSYM (QCxml_error_err_version_mismatch, ":err-version-mismatch");
  DEFSYM (QCxml_error_err_name_too_long, ":err-name-too-long");
  DEFSYM (QCxml_error_err_user_stop, ":err-user-stop");
  DEFSYM (QCxml_error_ns_err_xml_namespace, ":ns-err-xml-namespace");
  DEFSYM (QCxml_error_ns_err_undefined_namespace, ":ns-err-undefined-namespace");
  DEFSYM (QCxml_error_ns_err_qname, ":ns-err-qname");
  DEFSYM (QCxml_error_ns_err_attribute_redefined, ":ns-err-attribute-redefined");
  DEFSYM (QCxml_error_ns_err_empty, ":ns-err-empty");
  DEFSYM (QCxml_error_ns_err_colon, ":ns-err-colon");
  DEFSYM (QCxml_error_dtd_attribute_default, ":dtd-attribute-default");
  DEFSYM (QCxml_error_dtd_attribute_redefined, ":dtd-attribute-redefined");
  DEFSYM (QCxml_error_dtd_attribute_value, ":dtd-attribute-value");
  DEFSYM (QCxml_error_dtd_content_error, ":dtd-content-error");
  DEFSYM (QCxml_error_dtd_content_model, ":dtd-content-model");
  DEFSYM (QCxml_error_dtd_content_not_determinist, ":dtd-content-not-determinist");
  DEFSYM (QCxml_error_dtd_different_prefix, ":dtd-different-prefix");
  DEFSYM (QCxml_error_dtd_elem_default_namespace, ":dtd-elem-default-namespace");
  DEFSYM (QCxml_error_dtd_elem_namespace, ":dtd-elem-namespace");
  DEFSYM (QCxml_error_dtd_elem_redefined, ":dtd-elem-redefined");
  DEFSYM (QCxml_error_dtd_empty_notation, ":dtd-empty-notation");
  DEFSYM (QCxml_error_dtd_entity_type, ":dtd-entity-type");
  DEFSYM (QCxml_error_dtd_id_fixed, ":dtd-id-fixed");
  DEFSYM (QCxml_error_dtd_id_redefined, ":dtd-id-redefined");
  DEFSYM (QCxml_error_dtd_id_subset, ":dtd-id-subset");
  DEFSYM (QCxml_error_dtd_invalid_child, ":dtd-invalid-child");
  DEFSYM (QCxml_error_dtd_invalid_default, ":dtd-invalid-default");
  DEFSYM (QCxml_error_dtd_load_error, ":dtd-load-error");
  DEFSYM (QCxml_error_dtd_missing_attribute, ":dtd-missing-attribute");
  DEFSYM (QCxml_error_dtd_mixed_corrupt, ":dtd-mixed-corrupt");
  DEFSYM (QCxml_error_dtd_multiple_id, ":dtd-multiple-id");
  DEFSYM (QCxml_error_dtd_no_doc, ":dtd-no-doc");
  DEFSYM (QCxml_error_dtd_no_dtd, ":dtd-no-dtd");
  DEFSYM (QCxml_error_dtd_no_elem_name, ":dtd-no-elem-name");
  DEFSYM (QCxml_error_dtd_no_prefix, ":dtd-no-prefix");
  DEFSYM (QCxml_error_dtd_no_root, ":dtd-no-root");
  DEFSYM (QCxml_error_dtd_notation_redefined, ":dtd-notation-redefined");
  DEFSYM (QCxml_error_dtd_notation_value, ":dtd-notation-value");
  DEFSYM (QCxml_error_dtd_not_empty, ":dtd-not-empty");
  DEFSYM (QCxml_error_dtd_not_pcdata, ":dtd-not-pcdata");
  DEFSYM (QCxml_error_dtd_not_standalone, ":dtd-not-standalone");
  DEFSYM (QCxml_error_dtd_root_name, ":dtd-root-name");
  DEFSYM (QCxml_error_dtd_standalone_white_space, ":dtd-standalone-white-space");
  DEFSYM (QCxml_error_dtd_unknown_attribute, ":dtd-unknown-attribute");
  DEFSYM (QCxml_error_dtd_unknown_elem, ":dtd-unknown-elem");
  DEFSYM (QCxml_error_dtd_unknown_entity, ":dtd-unknown-entity");
  DEFSYM (QCxml_error_dtd_unknown_id, ":dtd-unknown-id");
  DEFSYM (QCxml_error_dtd_unknown_notation, ":dtd-unknown-notation");
  DEFSYM (QCxml_error_dtd_standalone_defaulted, ":dtd-standalone-defaulted");
  DEFSYM (QCxml_error_dtd_xmlid_value, ":dtd-xmlid-value");
  DEFSYM (QCxml_error_dtd_xmlid_type, ":dtd-xmlid-type");
  DEFSYM (QCxml_error_dtd_dup_token, ":dtd-dup-token");
  DEFSYM (QCxml_error_html_strucure_error, ":html-strucure-error");
  DEFSYM (QCxml_error_html_unknown_tag, ":html-unknown-tag");
  DEFSYM (QCxml_error_rngp_anyname_attr_ancestor, ":rngp-anyname-attr-ancestor");
  DEFSYM (QCxml_error_rngp_attr_conflict, ":rngp-attr-conflict");
  DEFSYM (QCxml_error_rngp_attribute_children, ":rngp-attribute-children");
  DEFSYM (QCxml_error_rngp_attribute_content, ":rngp-attribute-content");
  DEFSYM (QCxml_error_rngp_attribute_empty, ":rngp-attribute-empty");
  DEFSYM (QCxml_error_rngp_attribute_noop, ":rngp-attribute-noop");
  DEFSYM (QCxml_error_rngp_choice_content, ":rngp-choice-content");
  DEFSYM (QCxml_error_rngp_choice_empty, ":rngp-choice-empty");
  DEFSYM (QCxml_error_rngp_create_failure, ":rngp-create-failure");
  DEFSYM (QCxml_error_rngp_data_content, ":rngp-data-content");
  DEFSYM (QCxml_error_rngp_def_choice_and_interleave, ":rngp-def-choice-and-interleave");
  DEFSYM (QCxml_error_rngp_define_create_failed, ":rngp-define-create-failed");
  DEFSYM (QCxml_error_rngp_define_empty, ":rngp-define-empty");
  DEFSYM (QCxml_error_rngp_define_missing, ":rngp-define-missing");
  DEFSYM (QCxml_error_rngp_define_name_missing, ":rngp-define-name-missing");
  DEFSYM (QCxml_error_rngp_elem_content_empty, ":rngp-elem-content-empty");
  DEFSYM (QCxml_error_rngp_elem_content_error, ":rngp-elem-content-error");
  DEFSYM (QCxml_error_rngp_element_empty, ":rngp-element-empty");
  DEFSYM (QCxml_error_rngp_element_content, ":rngp-element-content");
  DEFSYM (QCxml_error_rngp_element_name, ":rngp-element-name");
  DEFSYM (QCxml_error_rngp_element_no_content, ":rngp-element-no-content");
  DEFSYM (QCxml_error_rngp_elem_text_conflict, ":rngp-elem-text-conflict");
  DEFSYM (QCxml_error_rngp_empty, ":rngp-empty");
  DEFSYM (QCxml_error_rngp_empty_construct, ":rngp-empty-construct");
  DEFSYM (QCxml_error_rngp_empty_content, ":rngp-empty-content");
  DEFSYM (QCxml_error_rngp_empty_not_empty, ":rngp-empty-not-empty");
  DEFSYM (QCxml_error_rngp_error_type_lib, ":rngp-error-type-lib");
  DEFSYM (QCxml_error_rngp_except_empty, ":rngp-except-empty");
  DEFSYM (QCxml_error_rngp_except_missing, ":rngp-except-missing");
  DEFSYM (QCxml_error_rngp_except_multiple, ":rngp-except-multiple");
  DEFSYM (QCxml_error_rngp_except_no_content, ":rngp-except-no-content");
  DEFSYM (QCxml_error_rngp_externalref_emtpy, ":rngp-externalref-emtpy");
  DEFSYM (QCxml_error_rngp_external_ref_failure, ":rngp-external-ref-failure");
  DEFSYM (QCxml_error_rngp_externalref_recurse, ":rngp-externalref-recurse");
  DEFSYM (QCxml_error_rngp_forbidden_attribute, ":rngp-forbidden-attribute");
  DEFSYM (QCxml_error_rngp_foreign_element, ":rngp-foreign-element");
  DEFSYM (QCxml_error_rngp_grammar_content, ":rngp-grammar-content");
  DEFSYM (QCxml_error_rngp_grammar_empty, ":rngp-grammar-empty");
  DEFSYM (QCxml_error_rngp_grammar_missing, ":rngp-grammar-missing");
  DEFSYM (QCxml_error_rngp_grammar_no_start, ":rngp-grammar-no-start");
  DEFSYM (QCxml_error_rngp_group_attr_conflict, ":rngp-group-attr-conflict");
  DEFSYM (QCxml_error_rngp_href_error, ":rngp-href-error");
  DEFSYM (QCxml_error_rngp_include_empty, ":rngp-include-empty");
  DEFSYM (QCxml_error_rngp_include_failure, ":rngp-include-failure");
  DEFSYM (QCxml_error_rngp_include_recurse, ":rngp-include-recurse");
  DEFSYM (QCxml_error_rngp_interleave_add, ":rngp-interleave-add");
  DEFSYM (QCxml_error_rngp_interleave_create_failed, ":rngp-interleave-create-failed");
  DEFSYM (QCxml_error_rngp_interleave_empty, ":rngp-interleave-empty");
  DEFSYM (QCxml_error_rngp_interleave_no_content, ":rngp-interleave-no-content");
  DEFSYM (QCxml_error_rngp_invalid_define_name, ":rngp-invalid-define-name");
  DEFSYM (QCxml_error_rngp_invalid_uri, ":rngp-invalid-uri");
  DEFSYM (QCxml_error_rngp_invalid_value, ":rngp-invalid-value");
  DEFSYM (QCxml_error_rngp_missing_href, ":rngp-missing-href");
  DEFSYM (QCxml_error_rngp_name_missing, ":rngp-name-missing");
  DEFSYM (QCxml_error_rngp_need_combine, ":rngp-need-combine");
  DEFSYM (QCxml_error_rngp_notallowed_not_empty, ":rngp-notallowed-not-empty");
  DEFSYM (QCxml_error_rngp_nsname_attr_ancestor, ":rngp-nsname-attr-ancestor");
  DEFSYM (QCxml_error_rngp_nsname_no_ns, ":rngp-nsname-no-ns");
  DEFSYM (QCxml_error_rngp_param_forbidden, ":rngp-param-forbidden");
  DEFSYM (QCxml_error_rngp_param_name_missing, ":rngp-param-name-missing");
  DEFSYM (QCxml_error_rngp_parentref_create_failed, ":rngp-parentref-create-failed");
  DEFSYM (QCxml_error_rngp_parentref_name_invalid, ":rngp-parentref-name-invalid");
  DEFSYM (QCxml_error_rngp_parentref_no_name, ":rngp-parentref-no-name");
  DEFSYM (QCxml_error_rngp_parentref_no_parent, ":rngp-parentref-no-parent");
  DEFSYM (QCxml_error_rngp_parentref_not_empty, ":rngp-parentref-not-empty");
  DEFSYM (QCxml_error_rngp_parse_error, ":rngp-parse-error");
  DEFSYM (QCxml_error_rngp_pat_anyname_except_anyname, ":rngp-pat-anyname-except-anyname");
  DEFSYM (QCxml_error_rngp_pat_attr_attr, ":rngp-pat-attr-attr");
  DEFSYM (QCxml_error_rngp_pat_attr_elem, ":rngp-pat-attr-elem");
  DEFSYM (QCxml_error_rngp_pat_data_except_attr, ":rngp-pat-data-except-attr");
  DEFSYM (QCxml_error_rngp_pat_data_except_elem, ":rngp-pat-data-except-elem");
  DEFSYM (QCxml_error_rngp_pat_data_except_empty, ":rngp-pat-data-except-empty");
  DEFSYM (QCxml_error_rngp_pat_data_except_group, ":rngp-pat-data-except-group");
  DEFSYM (QCxml_error_rngp_pat_data_except_interleave, ":rngp-pat-data-except-interleave");
  DEFSYM (QCxml_error_rngp_pat_data_except_list, ":rngp-pat-data-except-list");
  DEFSYM (QCxml_error_rngp_pat_data_except_onemore, ":rngp-pat-data-except-onemore");
  DEFSYM (QCxml_error_rngp_pat_data_except_ref, ":rngp-pat-data-except-ref");
  DEFSYM (QCxml_error_rngp_pat_data_except_text, ":rngp-pat-data-except-text");
  DEFSYM (QCxml_error_rngp_pat_list_attr, ":rngp-pat-list-attr");
  DEFSYM (QCxml_error_rngp_pat_list_elem, ":rngp-pat-list-elem");
  DEFSYM (QCxml_error_rngp_pat_list_interleave, ":rngp-pat-list-interleave");
  DEFSYM (QCxml_error_rngp_pat_list_list, ":rngp-pat-list-list");
  DEFSYM (QCxml_error_rngp_pat_list_ref, ":rngp-pat-list-ref");
  DEFSYM (QCxml_error_rngp_pat_list_text, ":rngp-pat-list-text");
  DEFSYM (QCxml_error_rngp_pat_nsname_except_anyname, ":rngp-pat-nsname-except-anyname");
  DEFSYM (QCxml_error_rngp_pat_nsname_except_nsname, ":rngp-pat-nsname-except-nsname");
  DEFSYM (QCxml_error_rngp_pat_onemore_group_attr, ":rngp-pat-onemore-group-attr");
  DEFSYM (QCxml_error_rngp_pat_onemore_interleave_attr, ":rngp-pat-onemore-interleave-attr");
  DEFSYM (QCxml_error_rngp_pat_start_attr, ":rngp-pat-start-attr");
  DEFSYM (QCxml_error_rngp_pat_start_data, ":rngp-pat-start-data");
  DEFSYM (QCxml_error_rngp_pat_start_empty, ":rngp-pat-start-empty");
  DEFSYM (QCxml_error_rngp_pat_start_group, ":rngp-pat-start-group");
  DEFSYM (QCxml_error_rngp_pat_start_interleave, ":rngp-pat-start-interleave");
  DEFSYM (QCxml_error_rngp_pat_start_list, ":rngp-pat-start-list");
  DEFSYM (QCxml_error_rngp_pat_start_onemore, ":rngp-pat-start-onemore");
  DEFSYM (QCxml_error_rngp_pat_start_text, ":rngp-pat-start-text");
  DEFSYM (QCxml_error_rngp_pat_start_value, ":rngp-pat-start-value");
  DEFSYM (QCxml_error_rngp_prefix_undefined, ":rngp-prefix-undefined");
  DEFSYM (QCxml_error_rngp_ref_create_failed, ":rngp-ref-create-failed");
  DEFSYM (QCxml_error_rngp_ref_cycle, ":rngp-ref-cycle");
  DEFSYM (QCxml_error_rngp_ref_name_invalid, ":rngp-ref-name-invalid");
  DEFSYM (QCxml_error_rngp_ref_no_def, ":rngp-ref-no-def");
  DEFSYM (QCxml_error_rngp_ref_no_name, ":rngp-ref-no-name");
  DEFSYM (QCxml_error_rngp_ref_not_empty, ":rngp-ref-not-empty");
  DEFSYM (QCxml_error_rngp_start_choice_and_interleave, ":rngp-start-choice-and-interleave");
  DEFSYM (QCxml_error_rngp_start_content, ":rngp-start-content");
  DEFSYM (QCxml_error_rngp_start_empty, ":rngp-start-empty");
  DEFSYM (QCxml_error_rngp_start_missing, ":rngp-start-missing");
  DEFSYM (QCxml_error_rngp_text_expected, ":rngp-text-expected");
  DEFSYM (QCxml_error_rngp_text_has_child, ":rngp-text-has-child");
  DEFSYM (QCxml_error_rngp_type_missing, ":rngp-type-missing");
  DEFSYM (QCxml_error_rngp_type_not_found, ":rngp-type-not-found");
  DEFSYM (QCxml_error_rngp_type_value, ":rngp-type-value");
  DEFSYM (QCxml_error_rngp_unknown_attribute, ":rngp-unknown-attribute");
  DEFSYM (QCxml_error_rngp_unknown_combine, ":rngp-unknown-combine");
  DEFSYM (QCxml_error_rngp_unknown_construct, ":rngp-unknown-construct");
  DEFSYM (QCxml_error_rngp_unknown_type_lib, ":rngp-unknown-type-lib");
  DEFSYM (QCxml_error_rngp_uri_fragment, ":rngp-uri-fragment");
  DEFSYM (QCxml_error_rngp_uri_not_absolute, ":rngp-uri-not-absolute");
  DEFSYM (QCxml_error_rngp_value_empty, ":rngp-value-empty");
  DEFSYM (QCxml_error_rngp_value_no_content, ":rngp-value-no-content");
  DEFSYM (QCxml_error_rngp_xmlns_name, ":rngp-xmlns-name");
  DEFSYM (QCxml_error_rngp_xml_ns, ":rngp-xml-ns");
  DEFSYM (QCxml_error_xpath_expression_ok, ":xpath-expression-ok");
  DEFSYM (QCxml_error_xpath_number_error, ":xpath-number-error");
  DEFSYM (QCxml_error_xpath_unfinished_literal_error, ":xpath-unfinished-literal-error");
  DEFSYM (QCxml_error_xpath_start_literal_error, ":xpath-start-literal-error");
  DEFSYM (QCxml_error_xpath_variable_ref_error, ":xpath-variable-ref-error");
  DEFSYM (QCxml_error_xpath_undef_variable_error, ":xpath-undef-variable-error");
  DEFSYM (QCxml_error_xpath_invalid_predicate_error, ":xpath-invalid-predicate-error");
  DEFSYM (QCxml_error_xpath_expr_error, ":xpath-expr-error");
  DEFSYM (QCxml_error_xpath_unclosed_error, ":xpath-unclosed-error");
  DEFSYM (QCxml_error_xpath_unknown_func_error, ":xpath-unknown-func-error");
  DEFSYM (QCxml_error_xpath_invalid_operand, ":xpath-invalid-operand");
  DEFSYM (QCxml_error_xpath_invalid_type, ":xpath-invalid-type");
  DEFSYM (QCxml_error_xpath_invalid_arity, ":xpath-invalid-arity");
  DEFSYM (QCxml_error_xpath_invalid_ctxt_size, ":xpath-invalid-ctxt-size");
  DEFSYM (QCxml_error_xpath_invalid_ctxt_position, ":xpath-invalid-ctxt-position");
  DEFSYM (QCxml_error_xpath_memory_error, ":xpath-memory-error");
  DEFSYM (QCxml_error_xptr_syntax_error, ":xptr-syntax-error");
  DEFSYM (QCxml_error_xptr_resource_error, ":xptr-resource-error");
  DEFSYM (QCxml_error_xptr_sub_resource_error, ":xptr-sub-resource-error");
  DEFSYM (QCxml_error_xpath_undef_prefix_error, ":xpath-undef-prefix-error");
  DEFSYM (QCxml_error_xpath_encoding_error, ":xpath-encoding-error");
  DEFSYM (QCxml_error_xpath_invalid_char_error, ":xpath-invalid-char-error");
  DEFSYM (QCxml_error_tree_invalid_hex, ":tree-invalid-hex");
  DEFSYM (QCxml_error_tree_invalid_dec, ":tree-invalid-dec");
  DEFSYM (QCxml_error_tree_unterminated_entity, ":tree-unterminated-entity");
  DEFSYM (QCxml_error_tree_not_utf8, ":tree-not-utf8");
  DEFSYM (QCxml_error_save_not_utf8, ":save-not-utf8");
  DEFSYM (QCxml_error_save_char_invalid, ":save-char-invalid");
  DEFSYM (QCxml_error_save_no_doctype, ":save-no-doctype");
  DEFSYM (QCxml_error_save_unknown_encoding, ":save-unknown-encoding");
  DEFSYM (QCxml_error_regexp_compile_error, ":regexp-compile-error");
  DEFSYM (QCxml_error_io_unknown, ":io-unknown");
  DEFSYM (QCxml_error_io_eacces, ":io-eacces");
  DEFSYM (QCxml_error_io_eagain, ":io-eagain");
  DEFSYM (QCxml_error_io_ebadf, ":io-ebadf");
  DEFSYM (QCxml_error_io_ebadmsg, ":io-ebadmsg");
  DEFSYM (QCxml_error_io_ebusy, ":io-ebusy");
  DEFSYM (QCxml_error_io_ecanceled, ":io-ecanceled");
  DEFSYM (QCxml_error_io_echild, ":io-echild");
  DEFSYM (QCxml_error_io_edeadlk, ":io-edeadlk");
  DEFSYM (QCxml_error_io_edom, ":io-edom");
  DEFSYM (QCxml_error_io_eexist, ":io-eexist");
  DEFSYM (QCxml_error_io_efault, ":io-efault");
  DEFSYM (QCxml_error_io_efbig, ":io-efbig");
  DEFSYM (QCxml_error_io_einprogress, ":io-einprogress");
  DEFSYM (QCxml_error_io_eintr, ":io-eintr");
  DEFSYM (QCxml_error_io_einval, ":io-einval");
  DEFSYM (QCxml_error_io_eio, ":io-eio");
  DEFSYM (QCxml_error_io_eisdir, ":io-eisdir");
  DEFSYM (QCxml_error_io_emfile, ":io-emfile");
  DEFSYM (QCxml_error_io_emlink, ":io-emlink");
  DEFSYM (QCxml_error_io_emsgsize, ":io-emsgsize");
  DEFSYM (QCxml_error_io_enametoolong, ":io-enametoolong");
  DEFSYM (QCxml_error_io_enfile, ":io-enfile");
  DEFSYM (QCxml_error_io_enodev, ":io-enodev");
  DEFSYM (QCxml_error_io_enoent, ":io-enoent");
  DEFSYM (QCxml_error_io_enoexec, ":io-enoexec");
  DEFSYM (QCxml_error_io_enolck, ":io-enolck");
  DEFSYM (QCxml_error_io_enomem, ":io-enomem");
  DEFSYM (QCxml_error_io_enospc, ":io-enospc");
  DEFSYM (QCxml_error_io_enosys, ":io-enosys");
  DEFSYM (QCxml_error_io_enotdir, ":io-enotdir");
  DEFSYM (QCxml_error_io_enotempty, ":io-enotempty");
  DEFSYM (QCxml_error_io_enotsup, ":io-enotsup");
  DEFSYM (QCxml_error_io_enotty, ":io-enotty");
  DEFSYM (QCxml_error_io_enxio, ":io-enxio");
  DEFSYM (QCxml_error_io_eperm, ":io-eperm");
  DEFSYM (QCxml_error_io_epipe, ":io-epipe");
  DEFSYM (QCxml_error_io_erange, ":io-erange");
  DEFSYM (QCxml_error_io_erofs, ":io-erofs");
  DEFSYM (QCxml_error_io_espipe, ":io-espipe");
  DEFSYM (QCxml_error_io_esrch, ":io-esrch");
  DEFSYM (QCxml_error_io_etimedout, ":io-etimedout");
  DEFSYM (QCxml_error_io_exdev, ":io-exdev");
  DEFSYM (QCxml_error_io_network_attempt, ":io-network-attempt");
  DEFSYM (QCxml_error_io_encoder, ":io-encoder");
  DEFSYM (QCxml_error_io_flush, ":io-flush");
  DEFSYM (QCxml_error_io_write, ":io-write");
  DEFSYM (QCxml_error_io_no_input, ":io-no-input");
  DEFSYM (QCxml_error_io_buffer_full, ":io-buffer-full");
  DEFSYM (QCxml_error_io_load_error, ":io-load-error");
  DEFSYM (QCxml_error_io_enotsock, ":io-enotsock");
  DEFSYM (QCxml_error_io_eisconn, ":io-eisconn");
  DEFSYM (QCxml_error_io_econnrefused, ":io-econnrefused");
  DEFSYM (QCxml_error_io_enetunreach, ":io-enetunreach");
  DEFSYM (QCxml_error_io_eaddrinuse, ":io-eaddrinuse");
  DEFSYM (QCxml_error_io_ealready, ":io-ealready");
  DEFSYM (QCxml_error_io_eafnosupport, ":io-eafnosupport");
  DEFSYM (QCxml_error_xinclude_recursion, ":xinclude-recursion");
  DEFSYM (QCxml_error_xinclude_parse_value, ":xinclude-parse-value");
  DEFSYM (QCxml_error_xinclude_entity_def_mismatch, ":xinclude-entity-def-mismatch");
  DEFSYM (QCxml_error_xinclude_no_href, ":xinclude-no-href");
  DEFSYM (QCxml_error_xinclude_no_fallback, ":xinclude-no-fallback");
  DEFSYM (QCxml_error_xinclude_href_uri, ":xinclude-href-uri");
  DEFSYM (QCxml_error_xinclude_text_fragment, ":xinclude-text-fragment");
  DEFSYM (QCxml_error_xinclude_text_document, ":xinclude-text-document");
  DEFSYM (QCxml_error_xinclude_invalid_char, ":xinclude-invalid-char");
  DEFSYM (QCxml_error_xinclude_build_failed, ":xinclude-build-failed");
  DEFSYM (QCxml_error_xinclude_unknown_encoding, ":xinclude-unknown-encoding");
  DEFSYM (QCxml_error_xinclude_multiple_root, ":xinclude-multiple-root");
  DEFSYM (QCxml_error_xinclude_xptr_failed, ":xinclude-xptr-failed");
  DEFSYM (QCxml_error_xinclude_xptr_result, ":xinclude-xptr-result");
  DEFSYM (QCxml_error_xinclude_include_in_include, ":xinclude-include-in-include");
  DEFSYM (QCxml_error_xinclude_fallbacks_in_include, ":xinclude-fallbacks-in-include");
  DEFSYM (QCxml_error_xinclude_fallback_not_in_include, ":xinclude-fallback-not-in-include");
  DEFSYM (QCxml_error_xinclude_deprecated_ns, ":xinclude-deprecated-ns");
  DEFSYM (QCxml_error_xinclude_fragment_id, ":xinclude-fragment-id");
  DEFSYM (QCxml_error_catalog_missing_attr, ":catalog-missing-attr");
  DEFSYM (QCxml_error_catalog_entry_broken, ":catalog-entry-broken");
  DEFSYM (QCxml_error_catalog_prefer_value, ":catalog-prefer-value");
  DEFSYM (QCxml_error_catalog_not_catalog, ":catalog-not-catalog");
  DEFSYM (QCxml_error_catalog_recursion, ":catalog-recursion");
  DEFSYM (QCxml_error_schemap_prefix_undefined, ":schemap-prefix-undefined");
  DEFSYM (QCxml_error_schemap_attrformdefault_value, ":schemap-attrformdefault-value");
  DEFSYM (QCxml_error_schemap_attrgrp_noname_noref, ":schemap-attrgrp-noname-noref");
  DEFSYM (QCxml_error_schemap_attr_noname_noref, ":schemap-attr-noname-noref");
  DEFSYM (QCxml_error_schemap_complextype_noname_noref, ":schemap-complextype-noname-noref");
  DEFSYM (QCxml_error_schemap_elemformdefault_value, ":schemap-elemformdefault-value");
  DEFSYM (QCxml_error_schemap_elem_noname_noref, ":schemap-elem-noname-noref");
  DEFSYM (QCxml_error_schemap_extension_no_base, ":schemap-extension-no-base");
  DEFSYM (QCxml_error_schemap_facet_no_value, ":schemap-facet-no-value");
  DEFSYM (QCxml_error_schemap_failed_build_import, ":schemap-failed-build-import");
  DEFSYM (QCxml_error_schemap_group_noname_noref, ":schemap-group-noname-noref");
  DEFSYM (QCxml_error_schemap_import_namespace_not_uri, ":schemap-import-namespace-not-uri");
  DEFSYM (QCxml_error_schemap_import_redefine_nsname, ":schemap-import-redefine-nsname");
  DEFSYM (QCxml_error_schemap_import_schema_not_uri, ":schemap-import-schema-not-uri");
  DEFSYM (QCxml_error_schemap_invalid_boolean, ":schemap-invalid-boolean");
  DEFSYM (QCxml_error_schemap_invalid_enum, ":schemap-invalid-enum");
  DEFSYM (QCxml_error_schemap_invalid_facet, ":schemap-invalid-facet");
  DEFSYM (QCxml_error_schemap_invalid_facet_value, ":schemap-invalid-facet-value");
  DEFSYM (QCxml_error_schemap_invalid_maxoccurs, ":schemap-invalid-maxoccurs");
  DEFSYM (QCxml_error_schemap_invalid_minoccurs, ":schemap-invalid-minoccurs");
  DEFSYM (QCxml_error_schemap_invalid_ref_and_subtype, ":schemap-invalid-ref-and-subtype");
  DEFSYM (QCxml_error_schemap_invalid_white_space, ":schemap-invalid-white-space");
  DEFSYM (QCxml_error_schemap_noattr_noref, ":schemap-noattr-noref");
  DEFSYM (QCxml_error_schemap_notation_no_name, ":schemap-notation-no-name");
  DEFSYM (QCxml_error_schemap_notype_noref, ":schemap-notype-noref");
  DEFSYM (QCxml_error_schemap_ref_and_subtype, ":schemap-ref-and-subtype");
  DEFSYM (QCxml_error_schemap_restriction_noname_noref, ":schemap-restriction-noname-noref");
  DEFSYM (QCxml_error_schemap_simpletype_noname, ":schemap-simpletype-noname");
  DEFSYM (QCxml_error_schemap_type_and_subtype, ":schemap-type-and-subtype");
  DEFSYM (QCxml_error_schemap_unknown_all_child, ":schemap-unknown-all-child");
  DEFSYM (QCxml_error_schemap_unknown_anyattribute_child, ":schemap-unknown-anyattribute-child");
  DEFSYM (QCxml_error_schemap_unknown_attr_child, ":schemap-unknown-attr-child");
  DEFSYM (QCxml_error_schemap_unknown_attrgrp_child, ":schemap-unknown-attrgrp-child");
  DEFSYM (QCxml_error_schemap_unknown_attribute_group, ":schemap-unknown-attribute-group");
  DEFSYM (QCxml_error_schemap_unknown_base_type, ":schemap-unknown-base-type");
  DEFSYM (QCxml_error_schemap_unknown_choice_child, ":schemap-unknown-choice-child");
  DEFSYM (QCxml_error_schemap_unknown_complexcontent_child, ":schemap-unknown-complexcontent-child");
  DEFSYM (QCxml_error_schemap_unknown_complextype_child, ":schemap-unknown-complextype-child");
  DEFSYM (QCxml_error_schemap_unknown_elem_child, ":schemap-unknown-elem-child");
  DEFSYM (QCxml_error_schemap_unknown_extension_child, ":schemap-unknown-extension-child");
  DEFSYM (QCxml_error_schemap_unknown_facet_child, ":schemap-unknown-facet-child");
  DEFSYM (QCxml_error_schemap_unknown_facet_type, ":schemap-unknown-facet-type");
  DEFSYM (QCxml_error_schemap_unknown_group_child, ":schemap-unknown-group-child");
  DEFSYM (QCxml_error_schemap_unknown_import_child, ":schemap-unknown-import-child");
  DEFSYM (QCxml_error_schemap_unknown_list_child, ":schemap-unknown-list-child");
  DEFSYM (QCxml_error_schemap_unknown_notation_child, ":schemap-unknown-notation-child");
  DEFSYM (QCxml_error_schemap_unknown_processcontent_child, ":schemap-unknown-processcontent-child");
  DEFSYM (QCxml_error_schemap_unknown_ref, ":schemap-unknown-ref");
  DEFSYM (QCxml_error_schemap_unknown_restriction_child, ":schemap-unknown-restriction-child");
  DEFSYM (QCxml_error_schemap_unknown_schemas_child, ":schemap-unknown-schemas-child");
  DEFSYM (QCxml_error_schemap_unknown_sequence_child, ":schemap-unknown-sequence-child");
  DEFSYM (QCxml_error_schemap_unknown_simplecontent_child, ":schemap-unknown-simplecontent-child");
  DEFSYM (QCxml_error_schemap_unknown_simpletype_child, ":schemap-unknown-simpletype-child");
  DEFSYM (QCxml_error_schemap_unknown_type, ":schemap-unknown-type");
  DEFSYM (QCxml_error_schemap_unknown_union_child, ":schemap-unknown-union-child");
  DEFSYM (QCxml_error_schemap_elem_default_fixed, ":schemap-elem-default-fixed");
  DEFSYM (QCxml_error_schemap_regexp_invalid, ":schemap-regexp-invalid");
  DEFSYM (QCxml_error_schemap_failed_load, ":schemap-failed-load");
  DEFSYM (QCxml_error_schemap_nothing_to_parse, ":schemap-nothing-to-parse");
  DEFSYM (QCxml_error_schemap_noroot, ":schemap-noroot");
  DEFSYM (QCxml_error_schemap_redefined_group, ":schemap-redefined-group");
  DEFSYM (QCxml_error_schemap_redefined_type, ":schemap-redefined-type");
  DEFSYM (QCxml_error_schemap_redefined_element, ":schemap-redefined-element");
  DEFSYM (QCxml_error_schemap_redefined_attrgroup, ":schemap-redefined-attrgroup");
  DEFSYM (QCxml_error_schemap_redefined_attr, ":schemap-redefined-attr");
  DEFSYM (QCxml_error_schemap_redefined_notation, ":schemap-redefined-notation");
  DEFSYM (QCxml_error_schemap_failed_parse, ":schemap-failed-parse");
  DEFSYM (QCxml_error_schemap_unknown_prefix, ":schemap-unknown-prefix");
  DEFSYM (QCxml_error_schemap_def_and_prefix, ":schemap-def-and-prefix");
  DEFSYM (QCxml_error_schemap_unknown_include_child, ":schemap-unknown-include-child");
  DEFSYM (QCxml_error_schemap_include_schema_not_uri, ":schemap-include-schema-not-uri");
  DEFSYM (QCxml_error_schemap_include_schema_no_uri, ":schemap-include-schema-no-uri");
  DEFSYM (QCxml_error_schemap_not_schema, ":schemap-not-schema");
  DEFSYM (QCxml_error_schemap_unknown_member_type, ":schemap-unknown-member-type");
  DEFSYM (QCxml_error_schemap_invalid_attr_use, ":schemap-invalid-attr-use");
  DEFSYM (QCxml_error_schemap_recursive, ":schemap-recursive");
  DEFSYM (QCxml_error_schemap_supernumerous_list_item_type, ":schemap-supernumerous-list-item-type");
  DEFSYM (QCxml_error_schemap_invalid_attr_combination, ":schemap-invalid-attr-combination");
  DEFSYM (QCxml_error_schemap_invalid_attr_inline_combination, ":schemap-invalid-attr-inline-combination");
  DEFSYM (QCxml_error_schemap_missing_simpletype_child, ":schemap-missing-simpletype-child");
  DEFSYM (QCxml_error_schemap_invalid_attr_name, ":schemap-invalid-attr-name");
  DEFSYM (QCxml_error_schemap_ref_and_content, ":schemap-ref-and-content");
  DEFSYM (QCxml_error_schemap_ct_props_correct_1, ":schemap-ct-props-correct-1");
  DEFSYM (QCxml_error_schemap_ct_props_correct_2, ":schemap-ct-props-correct-2");
  DEFSYM (QCxml_error_schemap_ct_props_correct_3, ":schemap-ct-props-correct-3");
  DEFSYM (QCxml_error_schemap_ct_props_correct_4, ":schemap-ct-props-correct-4");
  DEFSYM (QCxml_error_schemap_ct_props_correct_5, ":schemap-ct-props-correct-5");
  DEFSYM (QCxml_error_schemap_derivation_ok_restriction_1, ":schemap-derivation-ok-restriction-1");
  DEFSYM (QCxml_error_schemap_derivation_ok_restriction_2_1_1, ":schemap-derivation-ok-restriction-2-1-1");
  DEFSYM (QCxml_error_schemap_derivation_ok_restriction_2_1_2, ":schemap-derivation-ok-restriction-2-1-2");
  DEFSYM (QCxml_error_schemap_derivation_ok_restriction_2_2, ":schemap-derivation-ok-restriction-2-2");
  DEFSYM (QCxml_error_schemap_derivation_ok_restriction_3, ":schemap-derivation-ok-restriction-3");
  DEFSYM (QCxml_error_schemap_wildcard_invalid_ns_member, ":schemap-wildcard-invalid-ns-member");
  DEFSYM (QCxml_error_schemap_intersection_not_expressible, ":schemap-intersection-not-expressible");
  DEFSYM (QCxml_error_schemap_union_not_expressible, ":schemap-union-not-expressible");
  DEFSYM (QCxml_error_schemap_src_import_3_1, ":schemap-src-import-3-1");
  DEFSYM (QCxml_error_schemap_src_import_3_2, ":schemap-src-import-3-2");
  DEFSYM (QCxml_error_schemap_derivation_ok_restriction_4_1, ":schemap-derivation-ok-restriction-4-1");
  DEFSYM (QCxml_error_schemap_derivation_ok_restriction_4_2, ":schemap-derivation-ok-restriction-4-2");
  DEFSYM (QCxml_error_schemap_derivation_ok_restriction_4_3, ":schemap-derivation-ok-restriction-4-3");
  DEFSYM (QCxml_error_schemap_cos_ct_extends_1_3, ":schemap-cos-ct-extends-1-3");
  DEFSYM (QCxml_error_schemav_noroot, ":schemav-noroot");
  DEFSYM (QCxml_error_schemav_undeclaredelem, ":schemav-undeclaredelem");
  DEFSYM (QCxml_error_schemav_nottoplevel, ":schemav-nottoplevel");
  DEFSYM (QCxml_error_schemav_missing, ":schemav-missing");
  DEFSYM (QCxml_error_schemav_wrongelem, ":schemav-wrongelem");
  DEFSYM (QCxml_error_schemav_notype, ":schemav-notype");
  DEFSYM (QCxml_error_schemav_norollback, ":schemav-norollback");
  DEFSYM (QCxml_error_schemav_isabstract, ":schemav-isabstract");
  DEFSYM (QCxml_error_schemav_notempty, ":schemav-notempty");
  DEFSYM (QCxml_error_schemav_elemcont, ":schemav-elemcont");
  DEFSYM (QCxml_error_schemav_havedefault, ":schemav-havedefault");
  DEFSYM (QCxml_error_schemav_notnillable, ":schemav-notnillable");
  DEFSYM (QCxml_error_schemav_extracontent, ":schemav-extracontent");
  DEFSYM (QCxml_error_schemav_invalidattr, ":schemav-invalidattr");
  DEFSYM (QCxml_error_schemav_invalidelem, ":schemav-invalidelem");
  DEFSYM (QCxml_error_schemav_notdeterminist, ":schemav-notdeterminist");
  DEFSYM (QCxml_error_schemav_construct, ":schemav-construct");
  DEFSYM (QCxml_error_schemav_internal, ":schemav-internal");
  DEFSYM (QCxml_error_schemav_notsimple, ":schemav-notsimple");
  DEFSYM (QCxml_error_schemav_attrunknown, ":schemav-attrunknown");
  DEFSYM (QCxml_error_schemav_attrinvalid, ":schemav-attrinvalid");
  DEFSYM (QCxml_error_schemav_value, ":schemav-value");
  DEFSYM (QCxml_error_schemav_facet, ":schemav-facet");
  DEFSYM (QCxml_error_schemav_cvc_datatype_valid_1_2_1, ":schemav-cvc-datatype-valid-1-2-1");
  DEFSYM (QCxml_error_schemav_cvc_datatype_valid_1_2_2, ":schemav-cvc-datatype-valid-1-2-2");
  DEFSYM (QCxml_error_schemav_cvc_datatype_valid_1_2_3, ":schemav-cvc-datatype-valid-1-2-3");
  DEFSYM (QCxml_error_schemav_cvc_type_3_1_1, ":schemav-cvc-type-3-1-1");
  DEFSYM (QCxml_error_schemav_cvc_type_3_1_2, ":schemav-cvc-type-3-1-2");
  DEFSYM (QCxml_error_schemav_cvc_facet_valid, ":schemav-cvc-facet-valid");
  DEFSYM (QCxml_error_schemav_cvc_length_valid, ":schemav-cvc-length-valid");
  DEFSYM (QCxml_error_schemav_cvc_minlength_valid, ":schemav-cvc-minlength-valid");
  DEFSYM (QCxml_error_schemav_cvc_maxlength_valid, ":schemav-cvc-maxlength-valid");
  DEFSYM (QCxml_error_schemav_cvc_mininclusive_valid, ":schemav-cvc-mininclusive-valid");
  DEFSYM (QCxml_error_schemav_cvc_maxinclusive_valid, ":schemav-cvc-maxinclusive-valid");
  DEFSYM (QCxml_error_schemav_cvc_minexclusive_valid, ":schemav-cvc-minexclusive-valid");
  DEFSYM (QCxml_error_schemav_cvc_maxexclusive_valid, ":schemav-cvc-maxexclusive-valid");
  DEFSYM (QCxml_error_schemav_cvc_totaldigits_valid, ":schemav-cvc-totaldigits-valid");
  DEFSYM (QCxml_error_schemav_cvc_fractiondigits_valid, ":schemav-cvc-fractiondigits-valid");
  DEFSYM (QCxml_error_schemav_cvc_pattern_valid, ":schemav-cvc-pattern-valid");
  DEFSYM (QCxml_error_schemav_cvc_enumeration_valid, ":schemav-cvc-enumeration-valid");
  DEFSYM (QCxml_error_schemav_cvc_complex_type_2_1, ":schemav-cvc-complex-type-2-1");
  DEFSYM (QCxml_error_schemav_cvc_complex_type_2_2, ":schemav-cvc-complex-type-2-2");
  DEFSYM (QCxml_error_schemav_cvc_complex_type_2_3, ":schemav-cvc-complex-type-2-3");
  DEFSYM (QCxml_error_schemav_cvc_complex_type_2_4, ":schemav-cvc-complex-type-2-4");
  DEFSYM (QCxml_error_schemav_cvc_elt_1, ":schemav-cvc-elt-1");
  DEFSYM (QCxml_error_schemav_cvc_elt_2, ":schemav-cvc-elt-2");
  DEFSYM (QCxml_error_schemav_cvc_elt_3_1, ":schemav-cvc-elt-3-1");
  DEFSYM (QCxml_error_schemav_cvc_elt_3_2_1, ":schemav-cvc-elt-3-2-1");
  DEFSYM (QCxml_error_schemav_cvc_elt_3_2_2, ":schemav-cvc-elt-3-2-2");
  DEFSYM (QCxml_error_schemav_cvc_elt_4_1, ":schemav-cvc-elt-4-1");
  DEFSYM (QCxml_error_schemav_cvc_elt_4_2, ":schemav-cvc-elt-4-2");
  DEFSYM (QCxml_error_schemav_cvc_elt_4_3, ":schemav-cvc-elt-4-3");
  DEFSYM (QCxml_error_schemav_cvc_elt_5_1_1, ":schemav-cvc-elt-5-1-1");
  DEFSYM (QCxml_error_schemav_cvc_elt_5_1_2, ":schemav-cvc-elt-5-1-2");
  DEFSYM (QCxml_error_schemav_cvc_elt_5_2_1, ":schemav-cvc-elt-5-2-1");
  DEFSYM (QCxml_error_schemav_cvc_elt_5_2_2_1, ":schemav-cvc-elt-5-2-2-1");
  DEFSYM (QCxml_error_schemav_cvc_elt_5_2_2_2_1, ":schemav-cvc-elt-5-2-2-2-1");
  DEFSYM (QCxml_error_schemav_cvc_elt_5_2_2_2_2, ":schemav-cvc-elt-5-2-2-2-2");
  DEFSYM (QCxml_error_schemav_cvc_elt_6, ":schemav-cvc-elt-6");
  DEFSYM (QCxml_error_schemav_cvc_elt_7, ":schemav-cvc-elt-7");
  DEFSYM (QCxml_error_schemav_cvc_attribute_1, ":schemav-cvc-attribute-1");
  DEFSYM (QCxml_error_schemav_cvc_attribute_2, ":schemav-cvc-attribute-2");
  DEFSYM (QCxml_error_schemav_cvc_attribute_3, ":schemav-cvc-attribute-3");
  DEFSYM (QCxml_error_schemav_cvc_attribute_4, ":schemav-cvc-attribute-4");
  DEFSYM (QCxml_error_schemav_cvc_complex_type_3_1, ":schemav-cvc-complex-type-3-1");
  DEFSYM (QCxml_error_schemav_cvc_complex_type_3_2_1, ":schemav-cvc-complex-type-3-2-1");
  DEFSYM (QCxml_error_schemav_cvc_complex_type_3_2_2, ":schemav-cvc-complex-type-3-2-2");
  DEFSYM (QCxml_error_schemav_cvc_complex_type_4, ":schemav-cvc-complex-type-4");
  DEFSYM (QCxml_error_schemav_cvc_complex_type_5_1, ":schemav-cvc-complex-type-5-1");
  DEFSYM (QCxml_error_schemav_cvc_complex_type_5_2, ":schemav-cvc-complex-type-5-2");
  DEFSYM (QCxml_error_schemav_element_content, ":schemav-element-content");
  DEFSYM (QCxml_error_schemav_document_element_missing, ":schemav-document-element-missing");
  DEFSYM (QCxml_error_schemav_cvc_complex_type_1, ":schemav-cvc-complex-type-1");
  DEFSYM (QCxml_error_schemav_cvc_au, ":schemav-cvc-au");
  DEFSYM (QCxml_error_schemav_cvc_type_1, ":schemav-cvc-type-1");
  DEFSYM (QCxml_error_schemav_cvc_type_2, ":schemav-cvc-type-2");
  DEFSYM (QCxml_error_schemav_cvc_idc, ":schemav-cvc-idc");
  DEFSYM (QCxml_error_schemav_cvc_wildcard, ":schemav-cvc-wildcard");
  DEFSYM (QCxml_error_schemav_misc, ":schemav-misc");
  DEFSYM (QCxml_error_xptr_unknown_scheme, ":xptr-unknown-scheme");
  DEFSYM (QCxml_error_xptr_childseq_start, ":xptr-childseq-start");
  DEFSYM (QCxml_error_xptr_eval_failed, ":xptr-eval-failed");
  DEFSYM (QCxml_error_xptr_extra_objects, ":xptr-extra-objects");
  DEFSYM (QCxml_error_c14n_create_ctxt, ":c14n-create-ctxt");
  DEFSYM (QCxml_error_c14n_requires_utf8, ":c14n-requires-utf8");
  DEFSYM (QCxml_error_c14n_create_stack, ":c14n-create-stack");
  DEFSYM (QCxml_error_c14n_invalid_node, ":c14n-invalid-node");
  DEFSYM (QCxml_error_c14n_unknow_node, ":c14n-unknow-node");
  DEFSYM (QCxml_error_c14n_relative_namespace, ":c14n-relative-namespace");
  DEFSYM (QCxml_error_ftp_pasv_answer, ":ftp-pasv-answer");
  DEFSYM (QCxml_error_ftp_epsv_answer, ":ftp-epsv-answer");
  DEFSYM (QCxml_error_ftp_accnt, ":ftp-accnt");
  DEFSYM (QCxml_error_ftp_url_syntax, ":ftp-url-syntax");
  DEFSYM (QCxml_error_http_url_syntax, ":http-url-syntax");
  DEFSYM (QCxml_error_http_use_ip, ":http-use-ip");
  DEFSYM (QCxml_error_http_unknown_host, ":http-unknown-host");
  DEFSYM (QCxml_error_schemap_src_simple_type_1, ":schemap-src-simple-type-1");
  DEFSYM (QCxml_error_schemap_src_simple_type_2, ":schemap-src-simple-type-2");
  DEFSYM (QCxml_error_schemap_src_simple_type_3, ":schemap-src-simple-type-3");
  DEFSYM (QCxml_error_schemap_src_simple_type_4, ":schemap-src-simple-type-4");
  DEFSYM (QCxml_error_schemap_src_resolve, ":schemap-src-resolve");
  DEFSYM (QCxml_error_schemap_src_restriction_base_or_simpletype, ":schemap-src-restriction-base-or-simpletype");
  DEFSYM (QCxml_error_schemap_src_list_itemtype_or_simpletype, ":schemap-src-list-itemtype-or-simpletype");
  DEFSYM (QCxml_error_schemap_src_union_membertypes_or_simpletypes, ":schemap-src-union-membertypes-or-simpletypes");
  DEFSYM (QCxml_error_schemap_st_props_correct_1, ":schemap-st-props-correct-1");
  DEFSYM (QCxml_error_schemap_st_props_correct_2, ":schemap-st-props-correct-2");
  DEFSYM (QCxml_error_schemap_st_props_correct_3, ":schemap-st-props-correct-3");
  DEFSYM (QCxml_error_schemap_cos_st_restricts_1_1, ":schemap-cos-st-restricts-1-1");
  DEFSYM (QCxml_error_schemap_cos_st_restricts_1_2, ":schemap-cos-st-restricts-1-2");
  DEFSYM (QCxml_error_schemap_cos_st_restricts_1_3_1, ":schemap-cos-st-restricts-1-3-1");
  DEFSYM (QCxml_error_schemap_cos_st_restricts_1_3_2, ":schemap-cos-st-restricts-1-3-2");
  DEFSYM (QCxml_error_schemap_cos_st_restricts_2_1, ":schemap-cos-st-restricts-2-1");
  DEFSYM (QCxml_error_schemap_cos_st_restricts_2_3_1_1, ":schemap-cos-st-restricts-2-3-1-1");
  DEFSYM (QCxml_error_schemap_cos_st_restricts_2_3_1_2, ":schemap-cos-st-restricts-2-3-1-2");
  DEFSYM (QCxml_error_schemap_cos_st_restricts_2_3_2_1, ":schemap-cos-st-restricts-2-3-2-1");
  DEFSYM (QCxml_error_schemap_cos_st_restricts_2_3_2_2, ":schemap-cos-st-restricts-2-3-2-2");
  DEFSYM (QCxml_error_schemap_cos_st_restricts_2_3_2_3, ":schemap-cos-st-restricts-2-3-2-3");
  DEFSYM (QCxml_error_schemap_cos_st_restricts_2_3_2_4, ":schemap-cos-st-restricts-2-3-2-4");
  DEFSYM (QCxml_error_schemap_cos_st_restricts_2_3_2_5, ":schemap-cos-st-restricts-2-3-2-5");
  DEFSYM (QCxml_error_schemap_cos_st_restricts_3_1, ":schemap-cos-st-restricts-3-1");
  DEFSYM (QCxml_error_schemap_cos_st_restricts_3_3_1, ":schemap-cos-st-restricts-3-3-1");
  DEFSYM (QCxml_error_schemap_cos_st_restricts_3_3_1_2, ":schemap-cos-st-restricts-3-3-1-2");
  DEFSYM (QCxml_error_schemap_cos_st_restricts_3_3_2_2, ":schemap-cos-st-restricts-3-3-2-2");
  DEFSYM (QCxml_error_schemap_cos_st_restricts_3_3_2_1, ":schemap-cos-st-restricts-3-3-2-1");
  DEFSYM (QCxml_error_schemap_cos_st_restricts_3_3_2_3, ":schemap-cos-st-restricts-3-3-2-3");
  DEFSYM (QCxml_error_schemap_cos_st_restricts_3_3_2_4, ":schemap-cos-st-restricts-3-3-2-4");
  DEFSYM (QCxml_error_schemap_cos_st_restricts_3_3_2_5, ":schemap-cos-st-restricts-3-3-2-5");
  DEFSYM (QCxml_error_schemap_cos_st_derived_ok_2_1, ":schemap-cos-st-derived-ok-2-1");
  DEFSYM (QCxml_error_schemap_cos_st_derived_ok_2_2, ":schemap-cos-st-derived-ok-2-2");
  DEFSYM (QCxml_error_schemap_s4s_elem_not_allowed, ":schemap-s4s-elem-not-allowed");
  DEFSYM (QCxml_error_schemap_s4s_elem_missing, ":schemap-s4s-elem-missing");
  DEFSYM (QCxml_error_schemap_s4s_attr_not_allowed, ":schemap-s4s-attr-not-allowed");
  DEFSYM (QCxml_error_schemap_s4s_attr_missing, ":schemap-s4s-attr-missing");
  DEFSYM (QCxml_error_schemap_s4s_attr_invalid_value, ":schemap-s4s-attr-invalid-value");
  DEFSYM (QCxml_error_schemap_src_element_1, ":schemap-src-element-1");
  DEFSYM (QCxml_error_schemap_src_element_2_1, ":schemap-src-element-2-1");
  DEFSYM (QCxml_error_schemap_src_element_2_2, ":schemap-src-element-2-2");
  DEFSYM (QCxml_error_schemap_src_element_3, ":schemap-src-element-3");
  DEFSYM (QCxml_error_schemap_p_props_correct_1, ":schemap-p-props-correct-1");
  DEFSYM (QCxml_error_schemap_p_props_correct_2_1, ":schemap-p-props-correct-2-1");
  DEFSYM (QCxml_error_schemap_p_props_correct_2_2, ":schemap-p-props-correct-2-2");
  DEFSYM (QCxml_error_schemap_e_props_correct_2, ":schemap-e-props-correct-2");
  DEFSYM (QCxml_error_schemap_e_props_correct_3, ":schemap-e-props-correct-3");
  DEFSYM (QCxml_error_schemap_e_props_correct_4, ":schemap-e-props-correct-4");
  DEFSYM (QCxml_error_schemap_e_props_correct_5, ":schemap-e-props-correct-5");
  DEFSYM (QCxml_error_schemap_e_props_correct_6, ":schemap-e-props-correct-6");
  DEFSYM (QCxml_error_schemap_src_include, ":schemap-src-include");
  DEFSYM (QCxml_error_schemap_src_attribute_1, ":schemap-src-attribute-1");
  DEFSYM (QCxml_error_schemap_src_attribute_2, ":schemap-src-attribute-2");
  DEFSYM (QCxml_error_schemap_src_attribute_3_1, ":schemap-src-attribute-3-1");
  DEFSYM (QCxml_error_schemap_src_attribute_3_2, ":schemap-src-attribute-3-2");
  DEFSYM (QCxml_error_schemap_src_attribute_4, ":schemap-src-attribute-4");
  DEFSYM (QCxml_error_schemap_no_xmlns, ":schemap-no-xmlns");
  DEFSYM (QCxml_error_schemap_no_xsi, ":schemap-no-xsi");
  DEFSYM (QCxml_error_schemap_cos_valid_default_1, ":schemap-cos-valid-default-1");
  DEFSYM (QCxml_error_schemap_cos_valid_default_2_1, ":schemap-cos-valid-default-2-1");
  DEFSYM (QCxml_error_schemap_cos_valid_default_2_2_1, ":schemap-cos-valid-default-2-2-1");
  DEFSYM (QCxml_error_schemap_cos_valid_default_2_2_2, ":schemap-cos-valid-default-2-2-2");
  DEFSYM (QCxml_error_schemap_cvc_simple_type, ":schemap-cvc-simple-type");
  DEFSYM (QCxml_error_schemap_cos_ct_extends_1_1, ":schemap-cos-ct-extends-1-1");
  DEFSYM (QCxml_error_schemap_src_import_1_1, ":schemap-src-import-1-1");
  DEFSYM (QCxml_error_schemap_src_import_1_2, ":schemap-src-import-1-2");
  DEFSYM (QCxml_error_schemap_src_import_2, ":schemap-src-import-2");
  DEFSYM (QCxml_error_schemap_src_import_2_1, ":schemap-src-import-2-1");
  DEFSYM (QCxml_error_schemap_src_import_2_2, ":schemap-src-import-2-2");
  DEFSYM (QCxml_error_schemap_internal, ":schemap-internal");
  DEFSYM (QCxml_error_schemap_not_deterministic, ":schemap-not-deterministic");
  DEFSYM (QCxml_error_schemap_src_attribute_group_1, ":schemap-src-attribute-group-1");
  DEFSYM (QCxml_error_schemap_src_attribute_group_2, ":schemap-src-attribute-group-2");
  DEFSYM (QCxml_error_schemap_src_attribute_group_3, ":schemap-src-attribute-group-3");
  DEFSYM (QCxml_error_schemap_mg_props_correct_1, ":schemap-mg-props-correct-1");
  DEFSYM (QCxml_error_schemap_mg_props_correct_2, ":schemap-mg-props-correct-2");
  DEFSYM (QCxml_error_schemap_src_ct_1, ":schemap-src-ct-1");
  DEFSYM (QCxml_error_schemap_derivation_ok_restriction_2_1_3, ":schemap-derivation-ok-restriction-2-1-3");
  DEFSYM (QCxml_error_schemap_au_props_correct_2, ":schemap-au-props-correct-2");
  DEFSYM (QCxml_error_schemap_a_props_correct_2, ":schemap-a-props-correct-2");
  DEFSYM (QCxml_error_schemap_c_props_correct, ":schemap-c-props-correct");
  DEFSYM (QCxml_error_schemap_src_redefine, ":schemap-src-redefine");
  DEFSYM (QCxml_error_schemap_src_import, ":schemap-src-import");
  DEFSYM (QCxml_error_schemap_warn_skip_schema, ":schemap-warn-skip-schema");
  DEFSYM (QCxml_error_schemap_warn_unlocated_schema, ":schemap-warn-unlocated-schema");
  DEFSYM (QCxml_error_schemap_warn_attr_redecl_proh, ":schemap-warn-attr-redecl-proh");
  DEFSYM (QCxml_error_schemap_warn_attr_pointless_proh, ":schemap-warn-attr-pointless-proh");
  DEFSYM (QCxml_error_schemap_ag_props_correct, ":schemap-ag-props-correct");
  DEFSYM (QCxml_error_schemap_cos_ct_extends_1_2, ":schemap-cos-ct-extends-1-2");
  DEFSYM (QCxml_error_schemap_au_props_correct, ":schemap-au-props-correct");
  DEFSYM (QCxml_error_schemap_a_props_correct_3, ":schemap-a-props-correct-3");
  DEFSYM (QCxml_error_schemap_cos_all_limited, ":schemap-cos-all-limited");
  DEFSYM (QCxml_error_schematronv_assert, ":schematronv-assert");
  DEFSYM (QCxml_error_schematronv_report, ":schematronv-report");
  DEFSYM (QCxml_error_module_open, ":module-open");
  DEFSYM (QCxml_error_module_close, ":module-close");
  DEFSYM (QCxml_error_check_found_element, ":check-found-element");
  DEFSYM (QCxml_error_check_found_attribute, ":check-found-attribute");
  DEFSYM (QCxml_error_check_found_text, ":check-found-text");
  DEFSYM (QCxml_error_check_found_cdata, ":check-found-cdata");
  DEFSYM (QCxml_error_check_found_entityref, ":check-found-entityref");
  DEFSYM (QCxml_error_check_found_entity, ":check-found-entity");
  DEFSYM (QCxml_error_check_found_pi, ":check-found-pi");
  DEFSYM (QCxml_error_check_found_comment, ":check-found-comment");
  DEFSYM (QCxml_error_check_found_doctype, ":check-found-doctype");
  DEFSYM (QCxml_error_check_found_fragment, ":check-found-fragment");
  DEFSYM (QCxml_error_check_found_notation, ":check-found-notation");
  DEFSYM (QCxml_error_check_unknown_node, ":check-unknown-node");
  DEFSYM (QCxml_error_check_entity_type, ":check-entity-type");
  DEFSYM (QCxml_error_check_no_parent, ":check-no-parent");
  DEFSYM (QCxml_error_check_no_doc, ":check-no-doc");
  DEFSYM (QCxml_error_check_no_name, ":check-no-name");
  DEFSYM (QCxml_error_check_no_elem, ":check-no-elem");
  DEFSYM (QCxml_error_check_wrong_doc, ":check-wrong-doc");
  DEFSYM (QCxml_error_check_no_prev, ":check-no-prev");
  DEFSYM (QCxml_error_check_wrong_prev, ":check-wrong-prev");
  DEFSYM (QCxml_error_check_no_next, ":check-no-next");
  DEFSYM (QCxml_error_check_wrong_next, ":check-wrong-next");
  DEFSYM (QCxml_error_check_not_dtd, ":check-not-dtd");
  DEFSYM (QCxml_error_check_not_attr, ":check-not-attr");
  DEFSYM (QCxml_error_check_not_attr_decl, ":check-not-attr-decl");
  DEFSYM (QCxml_error_check_not_elem_decl, ":check-not-elem-decl");
  DEFSYM (QCxml_error_check_not_entity_decl, ":check-not-entity-decl");
  DEFSYM (QCxml_error_check_not_ns_decl, ":check-not-ns-decl");
  DEFSYM (QCxml_error_check_no_href, ":check-no-href");
  DEFSYM (QCxml_error_check_wrong_parent, ":check-wrong-parent");
  DEFSYM (QCxml_error_check_ns_scope, ":check-ns-scope");
  DEFSYM (QCxml_error_check_ns_ancestor, ":check-ns-ancestor");
  DEFSYM (QCxml_error_check_not_utf8, ":check-not-utf8");
  DEFSYM (QCxml_error_check_no_dict, ":check-no-dict");
  DEFSYM (QCxml_error_check_not_ncname, ":check-not-ncname");
  DEFSYM (QCxml_error_check_outside_dict, ":check-outside-dict");
  DEFSYM (QCxml_error_check_wrong_name, ":check-wrong-name");
  DEFSYM (QCxml_error_check_name_not_null, ":check-name-not-null");
  DEFSYM (QCxml_error_i18n_no_name, ":i18n-no-name");
  DEFSYM (QCxml_error_i18n_no_handler, ":i18n-no-handler");
  DEFSYM (QCxml_error_i18n_excess_handler, ":i18n-excess-handler");
  DEFSYM (QCxml_error_i18n_conv_failed, ":i18n-conv-failed");
  DEFSYM (QCxml_error_i18n_no_output, ":i18n-no-output");
  DEFSYM (QCxml_error_buf_overflow, ":buf-overflow");

}

static Lisp_Object
map_error (int code)
{
  switch (code)
    {
    case XML_ERR_OK:
      return QCxml_error_err_ok;
    case XML_ERR_INTERNAL_ERROR:
      return QCxml_error_err_internal_error;
    case XML_ERR_NO_MEMORY:
      return QCxml_error_err_no_memory;
    case XML_ERR_DOCUMENT_START:
      return QCxml_error_err_document_start;
    case XML_ERR_DOCUMENT_EMPTY:
      return QCxml_error_err_document_empty;
    case XML_ERR_DOCUMENT_END:
      return QCxml_error_err_document_end;
    case XML_ERR_INVALID_HEX_CHARREF:
      return QCxml_error_err_invalid_hex_charref;
    case XML_ERR_INVALID_DEC_CHARREF:
      return QCxml_error_err_invalid_dec_charref;
    case XML_ERR_INVALID_CHARREF:
      return QCxml_error_err_invalid_charref;
    case XML_ERR_INVALID_CHAR:
      return QCxml_error_err_invalid_char;
    case XML_ERR_CHARREF_AT_EOF:
      return QCxml_error_err_charref_at_eof;
    case XML_ERR_CHARREF_IN_PROLOG:
      return QCxml_error_err_charref_in_prolog;
    case XML_ERR_CHARREF_IN_EPILOG:
      return QCxml_error_err_charref_in_epilog;
    case XML_ERR_CHARREF_IN_DTD:
      return QCxml_error_err_charref_in_dtd;
    case XML_ERR_ENTITYREF_AT_EOF:
      return QCxml_error_err_entityref_at_eof;
    case XML_ERR_ENTITYREF_IN_PROLOG:
      return QCxml_error_err_entityref_in_prolog;
    case XML_ERR_ENTITYREF_IN_EPILOG:
      return QCxml_error_err_entityref_in_epilog;
    case XML_ERR_ENTITYREF_IN_DTD:
      return QCxml_error_err_entityref_in_dtd;
    case XML_ERR_PEREF_AT_EOF:
      return QCxml_error_err_peref_at_eof;
    case XML_ERR_PEREF_IN_PROLOG:
      return QCxml_error_err_peref_in_prolog;
    case XML_ERR_PEREF_IN_EPILOG:
      return QCxml_error_err_peref_in_epilog;
    case XML_ERR_PEREF_IN_INT_SUBSET:
      return QCxml_error_err_peref_in_int_subset;
    case XML_ERR_ENTITYREF_NO_NAME:
      return QCxml_error_err_entityref_no_name;
    case XML_ERR_ENTITYREF_SEMICOL_MISSING:
      return QCxml_error_err_entityref_semicol_missing;
    case XML_ERR_PEREF_NO_NAME:
      return QCxml_error_err_peref_no_name;
    case XML_ERR_PEREF_SEMICOL_MISSING:
      return QCxml_error_err_peref_semicol_missing;
    case XML_ERR_UNDECLARED_ENTITY:
      return QCxml_error_err_undeclared_entity;
    case XML_WAR_UNDECLARED_ENTITY:
      return QCxml_error_war_undeclared_entity;
    case XML_ERR_UNPARSED_ENTITY:
      return QCxml_error_err_unparsed_entity;
    case XML_ERR_ENTITY_IS_EXTERNAL:
      return QCxml_error_err_entity_is_external;
    case XML_ERR_ENTITY_IS_PARAMETER:
      return QCxml_error_err_entity_is_parameter;
    case XML_ERR_UNKNOWN_ENCODING:
      return QCxml_error_err_unknown_encoding;
    case XML_ERR_UNSUPPORTED_ENCODING:
      return QCxml_error_err_unsupported_encoding;
    case XML_ERR_STRING_NOT_STARTED:
      return QCxml_error_err_string_not_started;
    case XML_ERR_STRING_NOT_CLOSED:
      return QCxml_error_err_string_not_closed;
    case XML_ERR_NS_DECL_ERROR:
      return QCxml_error_err_ns_decl_error;
    case XML_ERR_ENTITY_NOT_STARTED:
      return QCxml_error_err_entity_not_started;
    case XML_ERR_ENTITY_NOT_FINISHED:
      return QCxml_error_err_entity_not_finished;
    case XML_ERR_LT_IN_ATTRIBUTE:
      return QCxml_error_err_lt_in_attribute;
    case XML_ERR_ATTRIBUTE_NOT_STARTED:
      return QCxml_error_err_attribute_not_started;
    case XML_ERR_ATTRIBUTE_NOT_FINISHED:
      return QCxml_error_err_attribute_not_finished;
    case XML_ERR_ATTRIBUTE_WITHOUT_VALUE:
      return QCxml_error_err_attribute_without_value;
    case XML_ERR_ATTRIBUTE_REDEFINED:
      return QCxml_error_err_attribute_redefined;
    case XML_ERR_LITERAL_NOT_STARTED:
      return QCxml_error_err_literal_not_started;
    case XML_ERR_LITERAL_NOT_FINISHED:
      return QCxml_error_err_literal_not_finished;
    case XML_ERR_COMMENT_NOT_FINISHED:
      return QCxml_error_err_comment_not_finished;
    case XML_ERR_PI_NOT_STARTED:
      return QCxml_error_err_pi_not_started;
    case XML_ERR_PI_NOT_FINISHED:
      return QCxml_error_err_pi_not_finished;
    case XML_ERR_NOTATION_NOT_STARTED:
      return QCxml_error_err_notation_not_started;
    case XML_ERR_NOTATION_NOT_FINISHED:
      return QCxml_error_err_notation_not_finished;
    case XML_ERR_ATTLIST_NOT_STARTED:
      return QCxml_error_err_attlist_not_started;
    case XML_ERR_ATTLIST_NOT_FINISHED:
      return QCxml_error_err_attlist_not_finished;
    case XML_ERR_MIXED_NOT_STARTED:
      return QCxml_error_err_mixed_not_started;
    case XML_ERR_MIXED_NOT_FINISHED:
      return QCxml_error_err_mixed_not_finished;
    case XML_ERR_ELEMCONTENT_NOT_STARTED:
      return QCxml_error_err_elemcontent_not_started;
    case XML_ERR_ELEMCONTENT_NOT_FINISHED:
      return QCxml_error_err_elemcontent_not_finished;
    case XML_ERR_XMLDECL_NOT_STARTED:
      return QCxml_error_err_xmldecl_not_started;
    case XML_ERR_XMLDECL_NOT_FINISHED:
      return QCxml_error_err_xmldecl_not_finished;
    case XML_ERR_CONDSEC_NOT_STARTED:
      return QCxml_error_err_condsec_not_started;
    case XML_ERR_CONDSEC_NOT_FINISHED:
      return QCxml_error_err_condsec_not_finished;
    case XML_ERR_EXT_SUBSET_NOT_FINISHED:
      return QCxml_error_err_ext_subset_not_finished;
    case XML_ERR_DOCTYPE_NOT_FINISHED:
      return QCxml_error_err_doctype_not_finished;
    case XML_ERR_MISPLACED_CDATA_END:
      return QCxml_error_err_misplaced_cdata_end;
    case XML_ERR_CDATA_NOT_FINISHED:
      return QCxml_error_err_cdata_not_finished;
    case XML_ERR_RESERVED_XML_NAME:
      return QCxml_error_err_reserved_xml_name;
    case XML_ERR_SPACE_REQUIRED:
      return QCxml_error_err_space_required;
    case XML_ERR_SEPARATOR_REQUIRED:
      return QCxml_error_err_separator_required;
    case XML_ERR_NMTOKEN_REQUIRED:
      return QCxml_error_err_nmtoken_required;
    case XML_ERR_NAME_REQUIRED:
      return QCxml_error_err_name_required;
    case XML_ERR_PCDATA_REQUIRED:
      return QCxml_error_err_pcdata_required;
    case XML_ERR_URI_REQUIRED:
      return QCxml_error_err_uri_required;
    case XML_ERR_PUBID_REQUIRED:
      return QCxml_error_err_pubid_required;
    case XML_ERR_LT_REQUIRED:
      return QCxml_error_err_lt_required;
    case XML_ERR_GT_REQUIRED:
      return QCxml_error_err_gt_required;
    case XML_ERR_LTSLASH_REQUIRED:
      return QCxml_error_err_ltslash_required;
    case XML_ERR_EQUAL_REQUIRED:
      return QCxml_error_err_equal_required;
    case XML_ERR_TAG_NAME_MISMATCH:
      return QCxml_error_err_tag_name_mismatch;
    case XML_ERR_TAG_NOT_FINISHED:
      return QCxml_error_err_tag_not_finished;
    case XML_ERR_STANDALONE_VALUE:
      return QCxml_error_err_standalone_value;
    case XML_ERR_ENCODING_NAME:
      return QCxml_error_err_encoding_name;
    case XML_ERR_HYPHEN_IN_COMMENT:
      return QCxml_error_err_hyphen_in_comment;
    case XML_ERR_INVALID_ENCODING:
      return QCxml_error_err_invalid_encoding;
    case XML_ERR_EXT_ENTITY_STANDALONE:
      return QCxml_error_err_ext_entity_standalone;
    case XML_ERR_CONDSEC_INVALID:
      return QCxml_error_err_condsec_invalid;
    case XML_ERR_VALUE_REQUIRED:
      return QCxml_error_err_value_required;
    case XML_ERR_NOT_WELL_BALANCED:
      return QCxml_error_err_not_well_balanced;
    case XML_ERR_EXTRA_CONTENT:
      return QCxml_error_err_extra_content;
    case XML_ERR_ENTITY_CHAR_ERROR:
      return QCxml_error_err_entity_char_error;
    case XML_ERR_ENTITY_PE_INTERNAL:
      return QCxml_error_err_entity_pe_internal;
    case XML_ERR_ENTITY_LOOP:
      return QCxml_error_err_entity_loop;
    case XML_ERR_ENTITY_BOUNDARY:
      return QCxml_error_err_entity_boundary;
    case XML_ERR_INVALID_URI:
      return QCxml_error_err_invalid_uri;
    case XML_ERR_URI_FRAGMENT:
      return QCxml_error_err_uri_fragment;
    case XML_WAR_CATALOG_PI:
      return QCxml_error_war_catalog_pi;
    case XML_ERR_NO_DTD:
      return QCxml_error_err_no_dtd;
    case XML_ERR_CONDSEC_INVALID_KEYWORD:
      return QCxml_error_err_condsec_invalid_keyword;
    case XML_ERR_VERSION_MISSING:
      return QCxml_error_err_version_missing;
    case XML_WAR_UNKNOWN_VERSION:
      return QCxml_error_war_unknown_version;
    case XML_WAR_LANG_VALUE:
      return QCxml_error_war_lang_value;
    case XML_WAR_NS_URI:
      return QCxml_error_war_ns_uri;
    case XML_WAR_NS_URI_RELATIVE:
      return QCxml_error_war_ns_uri_relative;
    case XML_ERR_MISSING_ENCODING:
      return QCxml_error_err_missing_encoding;
    case XML_WAR_SPACE_VALUE:
      return QCxml_error_war_space_value;
    case XML_ERR_NOT_STANDALONE:
      return QCxml_error_err_not_standalone;
    case XML_ERR_ENTITY_PROCESSING:
      return QCxml_error_err_entity_processing;
    case XML_ERR_NOTATION_PROCESSING:
      return QCxml_error_err_notation_processing;
    case XML_WAR_NS_COLUMN:
      return QCxml_error_war_ns_column;
    case XML_WAR_ENTITY_REDEFINED:
      return QCxml_error_war_entity_redefined;
    case XML_ERR_UNKNOWN_VERSION:
      return QCxml_error_err_unknown_version;
    case XML_ERR_VERSION_MISMATCH:
      return QCxml_error_err_version_mismatch;
    case XML_ERR_NAME_TOO_LONG:
      return QCxml_error_err_name_too_long;
    case XML_ERR_USER_STOP:
      return QCxml_error_err_user_stop;
    case XML_NS_ERR_XML_NAMESPACE:
      return QCxml_error_ns_err_xml_namespace;
    case XML_NS_ERR_UNDEFINED_NAMESPACE:
      return QCxml_error_ns_err_undefined_namespace;
    case XML_NS_ERR_QNAME:
      return QCxml_error_ns_err_qname;
    case XML_NS_ERR_ATTRIBUTE_REDEFINED:
      return QCxml_error_ns_err_attribute_redefined;
    case XML_NS_ERR_EMPTY:
      return QCxml_error_ns_err_empty;
    case XML_NS_ERR_COLON:
      return QCxml_error_ns_err_colon;
    case XML_DTD_ATTRIBUTE_DEFAULT:
      return QCxml_error_dtd_attribute_default;
    case XML_DTD_ATTRIBUTE_REDEFINED:
      return QCxml_error_dtd_attribute_redefined;
    case XML_DTD_ATTRIBUTE_VALUE:
      return QCxml_error_dtd_attribute_value;
    case XML_DTD_CONTENT_ERROR:
      return QCxml_error_dtd_content_error;
    case XML_DTD_CONTENT_MODEL:
      return QCxml_error_dtd_content_model;
    case XML_DTD_CONTENT_NOT_DETERMINIST:
      return QCxml_error_dtd_content_not_determinist;
    case XML_DTD_DIFFERENT_PREFIX:
      return QCxml_error_dtd_different_prefix;
    case XML_DTD_ELEM_DEFAULT_NAMESPACE:
      return QCxml_error_dtd_elem_default_namespace;
    case XML_DTD_ELEM_NAMESPACE:
      return QCxml_error_dtd_elem_namespace;
    case XML_DTD_ELEM_REDEFINED:
      return QCxml_error_dtd_elem_redefined;
    case XML_DTD_EMPTY_NOTATION:
      return QCxml_error_dtd_empty_notation;
    case XML_DTD_ENTITY_TYPE:
      return QCxml_error_dtd_entity_type;
    case XML_DTD_ID_FIXED:
      return QCxml_error_dtd_id_fixed;
    case XML_DTD_ID_REDEFINED:
      return QCxml_error_dtd_id_redefined;
    case XML_DTD_ID_SUBSET:
      return QCxml_error_dtd_id_subset;
    case XML_DTD_INVALID_CHILD:
      return QCxml_error_dtd_invalid_child;
    case XML_DTD_INVALID_DEFAULT:
      return QCxml_error_dtd_invalid_default;
    case XML_DTD_LOAD_ERROR:
      return QCxml_error_dtd_load_error;
    case XML_DTD_MISSING_ATTRIBUTE:
      return QCxml_error_dtd_missing_attribute;
    case XML_DTD_MIXED_CORRUPT:
      return QCxml_error_dtd_mixed_corrupt;
    case XML_DTD_MULTIPLE_ID:
      return QCxml_error_dtd_multiple_id;
    case XML_DTD_NO_DOC:
      return QCxml_error_dtd_no_doc;
    case XML_DTD_NO_DTD:
      return QCxml_error_dtd_no_dtd;
    case XML_DTD_NO_ELEM_NAME:
      return QCxml_error_dtd_no_elem_name;
    case XML_DTD_NO_PREFIX:
      return QCxml_error_dtd_no_prefix;
    case XML_DTD_NO_ROOT:
      return QCxml_error_dtd_no_root;
    case XML_DTD_NOTATION_REDEFINED:
      return QCxml_error_dtd_notation_redefined;
    case XML_DTD_NOTATION_VALUE:
      return QCxml_error_dtd_notation_value;
    case XML_DTD_NOT_EMPTY:
      return QCxml_error_dtd_not_empty;
    case XML_DTD_NOT_PCDATA:
      return QCxml_error_dtd_not_pcdata;
    case XML_DTD_NOT_STANDALONE:
      return QCxml_error_dtd_not_standalone;
    case XML_DTD_ROOT_NAME:
      return QCxml_error_dtd_root_name;
    case XML_DTD_STANDALONE_WHITE_SPACE:
      return QCxml_error_dtd_standalone_white_space;
    case XML_DTD_UNKNOWN_ATTRIBUTE:
      return QCxml_error_dtd_unknown_attribute;
    case XML_DTD_UNKNOWN_ELEM:
      return QCxml_error_dtd_unknown_elem;
    case XML_DTD_UNKNOWN_ENTITY:
      return QCxml_error_dtd_unknown_entity;
    case XML_DTD_UNKNOWN_ID:
      return QCxml_error_dtd_unknown_id;
    case XML_DTD_UNKNOWN_NOTATION:
      return QCxml_error_dtd_unknown_notation;
    case XML_DTD_STANDALONE_DEFAULTED:
      return QCxml_error_dtd_standalone_defaulted;
    case XML_DTD_XMLID_VALUE:
      return QCxml_error_dtd_xmlid_value;
    case XML_DTD_XMLID_TYPE:
      return QCxml_error_dtd_xmlid_type;
    case XML_DTD_DUP_TOKEN:
      return QCxml_error_dtd_dup_token;
    case XML_HTML_STRUCURE_ERROR:
      return QCxml_error_html_strucure_error;
    case XML_HTML_UNKNOWN_TAG:
      return QCxml_error_html_unknown_tag;
    case XML_RNGP_ANYNAME_ATTR_ANCESTOR:
      return QCxml_error_rngp_anyname_attr_ancestor;
    case XML_RNGP_ATTR_CONFLICT:
      return QCxml_error_rngp_attr_conflict;
    case XML_RNGP_ATTRIBUTE_CHILDREN:
      return QCxml_error_rngp_attribute_children;
    case XML_RNGP_ATTRIBUTE_CONTENT:
      return QCxml_error_rngp_attribute_content;
    case XML_RNGP_ATTRIBUTE_EMPTY:
      return QCxml_error_rngp_attribute_empty;
    case XML_RNGP_ATTRIBUTE_NOOP:
      return QCxml_error_rngp_attribute_noop;
    case XML_RNGP_CHOICE_CONTENT:
      return QCxml_error_rngp_choice_content;
    case XML_RNGP_CHOICE_EMPTY:
      return QCxml_error_rngp_choice_empty;
    case XML_RNGP_CREATE_FAILURE:
      return QCxml_error_rngp_create_failure;
    case XML_RNGP_DATA_CONTENT:
      return QCxml_error_rngp_data_content;
    case XML_RNGP_DEF_CHOICE_AND_INTERLEAVE:
      return QCxml_error_rngp_def_choice_and_interleave;
    case XML_RNGP_DEFINE_CREATE_FAILED:
      return QCxml_error_rngp_define_create_failed;
    case XML_RNGP_DEFINE_EMPTY:
      return QCxml_error_rngp_define_empty;
    case XML_RNGP_DEFINE_MISSING:
      return QCxml_error_rngp_define_missing;
    case XML_RNGP_DEFINE_NAME_MISSING:
      return QCxml_error_rngp_define_name_missing;
    case XML_RNGP_ELEM_CONTENT_EMPTY:
      return QCxml_error_rngp_elem_content_empty;
    case XML_RNGP_ELEM_CONTENT_ERROR:
      return QCxml_error_rngp_elem_content_error;
    case XML_RNGP_ELEMENT_EMPTY:
      return QCxml_error_rngp_element_empty;
    case XML_RNGP_ELEMENT_CONTENT:
      return QCxml_error_rngp_element_content;
    case XML_RNGP_ELEMENT_NAME:
      return QCxml_error_rngp_element_name;
    case XML_RNGP_ELEMENT_NO_CONTENT:
      return QCxml_error_rngp_element_no_content;
    case XML_RNGP_ELEM_TEXT_CONFLICT:
      return QCxml_error_rngp_elem_text_conflict;
    case XML_RNGP_EMPTY:
      return QCxml_error_rngp_empty;
    case XML_RNGP_EMPTY_CONSTRUCT:
      return QCxml_error_rngp_empty_construct;
    case XML_RNGP_EMPTY_CONTENT:
      return QCxml_error_rngp_empty_content;
    case XML_RNGP_EMPTY_NOT_EMPTY:
      return QCxml_error_rngp_empty_not_empty;
    case XML_RNGP_ERROR_TYPE_LIB:
      return QCxml_error_rngp_error_type_lib;
    case XML_RNGP_EXCEPT_EMPTY:
      return QCxml_error_rngp_except_empty;
    case XML_RNGP_EXCEPT_MISSING:
      return QCxml_error_rngp_except_missing;
    case XML_RNGP_EXCEPT_MULTIPLE:
      return QCxml_error_rngp_except_multiple;
    case XML_RNGP_EXCEPT_NO_CONTENT:
      return QCxml_error_rngp_except_no_content;
    case XML_RNGP_EXTERNALREF_EMTPY:
      return QCxml_error_rngp_externalref_emtpy;
    case XML_RNGP_EXTERNAL_REF_FAILURE:
      return QCxml_error_rngp_external_ref_failure;
    case XML_RNGP_EXTERNALREF_RECURSE:
      return QCxml_error_rngp_externalref_recurse;
    case XML_RNGP_FORBIDDEN_ATTRIBUTE:
      return QCxml_error_rngp_forbidden_attribute;
    case XML_RNGP_FOREIGN_ELEMENT:
      return QCxml_error_rngp_foreign_element;
    case XML_RNGP_GRAMMAR_CONTENT:
      return QCxml_error_rngp_grammar_content;
    case XML_RNGP_GRAMMAR_EMPTY:
      return QCxml_error_rngp_grammar_empty;
    case XML_RNGP_GRAMMAR_MISSING:
      return QCxml_error_rngp_grammar_missing;
    case XML_RNGP_GRAMMAR_NO_START:
      return QCxml_error_rngp_grammar_no_start;
    case XML_RNGP_GROUP_ATTR_CONFLICT:
      return QCxml_error_rngp_group_attr_conflict;
    case XML_RNGP_HREF_ERROR:
      return QCxml_error_rngp_href_error;
    case XML_RNGP_INCLUDE_EMPTY:
      return QCxml_error_rngp_include_empty;
    case XML_RNGP_INCLUDE_FAILURE:
      return QCxml_error_rngp_include_failure;
    case XML_RNGP_INCLUDE_RECURSE:
      return QCxml_error_rngp_include_recurse;
    case XML_RNGP_INTERLEAVE_ADD:
      return QCxml_error_rngp_interleave_add;
    case XML_RNGP_INTERLEAVE_CREATE_FAILED:
      return QCxml_error_rngp_interleave_create_failed;
    case XML_RNGP_INTERLEAVE_EMPTY:
      return QCxml_error_rngp_interleave_empty;
    case XML_RNGP_INTERLEAVE_NO_CONTENT:
      return QCxml_error_rngp_interleave_no_content;
    case XML_RNGP_INVALID_DEFINE_NAME:
      return QCxml_error_rngp_invalid_define_name;
    case XML_RNGP_INVALID_URI:
      return QCxml_error_rngp_invalid_uri;
    case XML_RNGP_INVALID_VALUE:
      return QCxml_error_rngp_invalid_value;
    case XML_RNGP_MISSING_HREF:
      return QCxml_error_rngp_missing_href;
    case XML_RNGP_NAME_MISSING:
      return QCxml_error_rngp_name_missing;
    case XML_RNGP_NEED_COMBINE:
      return QCxml_error_rngp_need_combine;
    case XML_RNGP_NOTALLOWED_NOT_EMPTY:
      return QCxml_error_rngp_notallowed_not_empty;
    case XML_RNGP_NSNAME_ATTR_ANCESTOR:
      return QCxml_error_rngp_nsname_attr_ancestor;
    case XML_RNGP_NSNAME_NO_NS:
      return QCxml_error_rngp_nsname_no_ns;
    case XML_RNGP_PARAM_FORBIDDEN:
      return QCxml_error_rngp_param_forbidden;
    case XML_RNGP_PARAM_NAME_MISSING:
      return QCxml_error_rngp_param_name_missing;
    case XML_RNGP_PARENTREF_CREATE_FAILED:
      return QCxml_error_rngp_parentref_create_failed;
    case XML_RNGP_PARENTREF_NAME_INVALID:
      return QCxml_error_rngp_parentref_name_invalid;
    case XML_RNGP_PARENTREF_NO_NAME:
      return QCxml_error_rngp_parentref_no_name;
    case XML_RNGP_PARENTREF_NO_PARENT:
      return QCxml_error_rngp_parentref_no_parent;
    case XML_RNGP_PARENTREF_NOT_EMPTY:
      return QCxml_error_rngp_parentref_not_empty;
    case XML_RNGP_PARSE_ERROR:
      return QCxml_error_rngp_parse_error;
    case XML_RNGP_PAT_ANYNAME_EXCEPT_ANYNAME:
      return QCxml_error_rngp_pat_anyname_except_anyname;
    case XML_RNGP_PAT_ATTR_ATTR:
      return QCxml_error_rngp_pat_attr_attr;
    case XML_RNGP_PAT_ATTR_ELEM:
      return QCxml_error_rngp_pat_attr_elem;
    case XML_RNGP_PAT_DATA_EXCEPT_ATTR:
      return QCxml_error_rngp_pat_data_except_attr;
    case XML_RNGP_PAT_DATA_EXCEPT_ELEM:
      return QCxml_error_rngp_pat_data_except_elem;
    case XML_RNGP_PAT_DATA_EXCEPT_EMPTY:
      return QCxml_error_rngp_pat_data_except_empty;
    case XML_RNGP_PAT_DATA_EXCEPT_GROUP:
      return QCxml_error_rngp_pat_data_except_group;
    case XML_RNGP_PAT_DATA_EXCEPT_INTERLEAVE:
      return QCxml_error_rngp_pat_data_except_interleave;
    case XML_RNGP_PAT_DATA_EXCEPT_LIST:
      return QCxml_error_rngp_pat_data_except_list;
    case XML_RNGP_PAT_DATA_EXCEPT_ONEMORE:
      return QCxml_error_rngp_pat_data_except_onemore;
    case XML_RNGP_PAT_DATA_EXCEPT_REF:
      return QCxml_error_rngp_pat_data_except_ref;
    case XML_RNGP_PAT_DATA_EXCEPT_TEXT:
      return QCxml_error_rngp_pat_data_except_text;
    case XML_RNGP_PAT_LIST_ATTR:
      return QCxml_error_rngp_pat_list_attr;
    case XML_RNGP_PAT_LIST_ELEM:
      return QCxml_error_rngp_pat_list_elem;
    case XML_RNGP_PAT_LIST_INTERLEAVE:
      return QCxml_error_rngp_pat_list_interleave;
    case XML_RNGP_PAT_LIST_LIST:
      return QCxml_error_rngp_pat_list_list;
    case XML_RNGP_PAT_LIST_REF:
      return QCxml_error_rngp_pat_list_ref;
    case XML_RNGP_PAT_LIST_TEXT:
      return QCxml_error_rngp_pat_list_text;
    case XML_RNGP_PAT_NSNAME_EXCEPT_ANYNAME:
      return QCxml_error_rngp_pat_nsname_except_anyname;
    case XML_RNGP_PAT_NSNAME_EXCEPT_NSNAME:
      return QCxml_error_rngp_pat_nsname_except_nsname;
    case XML_RNGP_PAT_ONEMORE_GROUP_ATTR:
      return QCxml_error_rngp_pat_onemore_group_attr;
    case XML_RNGP_PAT_ONEMORE_INTERLEAVE_ATTR:
      return QCxml_error_rngp_pat_onemore_interleave_attr;
    case XML_RNGP_PAT_START_ATTR:
      return QCxml_error_rngp_pat_start_attr;
    case XML_RNGP_PAT_START_DATA:
      return QCxml_error_rngp_pat_start_data;
    case XML_RNGP_PAT_START_EMPTY:
      return QCxml_error_rngp_pat_start_empty;
    case XML_RNGP_PAT_START_GROUP:
      return QCxml_error_rngp_pat_start_group;
    case XML_RNGP_PAT_START_INTERLEAVE:
      return QCxml_error_rngp_pat_start_interleave;
    case XML_RNGP_PAT_START_LIST:
      return QCxml_error_rngp_pat_start_list;
    case XML_RNGP_PAT_START_ONEMORE:
      return QCxml_error_rngp_pat_start_onemore;
    case XML_RNGP_PAT_START_TEXT:
      return QCxml_error_rngp_pat_start_text;
    case XML_RNGP_PAT_START_VALUE:
      return QCxml_error_rngp_pat_start_value;
    case XML_RNGP_PREFIX_UNDEFINED:
      return QCxml_error_rngp_prefix_undefined;
    case XML_RNGP_REF_CREATE_FAILED:
      return QCxml_error_rngp_ref_create_failed;
    case XML_RNGP_REF_CYCLE:
      return QCxml_error_rngp_ref_cycle;
    case XML_RNGP_REF_NAME_INVALID:
      return QCxml_error_rngp_ref_name_invalid;
    case XML_RNGP_REF_NO_DEF:
      return QCxml_error_rngp_ref_no_def;
    case XML_RNGP_REF_NO_NAME:
      return QCxml_error_rngp_ref_no_name;
    case XML_RNGP_REF_NOT_EMPTY:
      return QCxml_error_rngp_ref_not_empty;
    case XML_RNGP_START_CHOICE_AND_INTERLEAVE:
      return QCxml_error_rngp_start_choice_and_interleave;
    case XML_RNGP_START_CONTENT:
      return QCxml_error_rngp_start_content;
    case XML_RNGP_START_EMPTY:
      return QCxml_error_rngp_start_empty;
    case XML_RNGP_START_MISSING:
      return QCxml_error_rngp_start_missing;
    case XML_RNGP_TEXT_EXPECTED:
      return QCxml_error_rngp_text_expected;
    case XML_RNGP_TEXT_HAS_CHILD:
      return QCxml_error_rngp_text_has_child;
    case XML_RNGP_TYPE_MISSING:
      return QCxml_error_rngp_type_missing;
    case XML_RNGP_TYPE_NOT_FOUND:
      return QCxml_error_rngp_type_not_found;
    case XML_RNGP_TYPE_VALUE:
      return QCxml_error_rngp_type_value;
    case XML_RNGP_UNKNOWN_ATTRIBUTE:
      return QCxml_error_rngp_unknown_attribute;
    case XML_RNGP_UNKNOWN_COMBINE:
      return QCxml_error_rngp_unknown_combine;
    case XML_RNGP_UNKNOWN_CONSTRUCT:
      return QCxml_error_rngp_unknown_construct;
    case XML_RNGP_UNKNOWN_TYPE_LIB:
      return QCxml_error_rngp_unknown_type_lib;
    case XML_RNGP_URI_FRAGMENT:
      return QCxml_error_rngp_uri_fragment;
    case XML_RNGP_URI_NOT_ABSOLUTE:
      return QCxml_error_rngp_uri_not_absolute;
    case XML_RNGP_VALUE_EMPTY:
      return QCxml_error_rngp_value_empty;
    case XML_RNGP_VALUE_NO_CONTENT:
      return QCxml_error_rngp_value_no_content;
    case XML_RNGP_XMLNS_NAME:
      return QCxml_error_rngp_xmlns_name;
    case XML_RNGP_XML_NS:
      return QCxml_error_rngp_xml_ns;
    case XML_XPATH_EXPRESSION_OK:
      return QCxml_error_xpath_expression_ok;
    case XML_XPATH_NUMBER_ERROR:
      return QCxml_error_xpath_number_error;
    case XML_XPATH_UNFINISHED_LITERAL_ERROR:
      return QCxml_error_xpath_unfinished_literal_error;
    case XML_XPATH_START_LITERAL_ERROR:
      return QCxml_error_xpath_start_literal_error;
    case XML_XPATH_VARIABLE_REF_ERROR:
      return QCxml_error_xpath_variable_ref_error;
    case XML_XPATH_UNDEF_VARIABLE_ERROR:
      return QCxml_error_xpath_undef_variable_error;
    case XML_XPATH_INVALID_PREDICATE_ERROR:
      return QCxml_error_xpath_invalid_predicate_error;
    case XML_XPATH_EXPR_ERROR:
      return QCxml_error_xpath_expr_error;
    case XML_XPATH_UNCLOSED_ERROR:
      return QCxml_error_xpath_unclosed_error;
    case XML_XPATH_UNKNOWN_FUNC_ERROR:
      return QCxml_error_xpath_unknown_func_error;
    case XML_XPATH_INVALID_OPERAND:
      return QCxml_error_xpath_invalid_operand;
    case XML_XPATH_INVALID_TYPE:
      return QCxml_error_xpath_invalid_type;
    case XML_XPATH_INVALID_ARITY:
      return QCxml_error_xpath_invalid_arity;
    case XML_XPATH_INVALID_CTXT_SIZE:
      return QCxml_error_xpath_invalid_ctxt_size;
    case XML_XPATH_INVALID_CTXT_POSITION:
      return QCxml_error_xpath_invalid_ctxt_position;
    case XML_XPATH_MEMORY_ERROR:
      return QCxml_error_xpath_memory_error;
    case XML_XPTR_SYNTAX_ERROR:
      return QCxml_error_xptr_syntax_error;
    case XML_XPTR_RESOURCE_ERROR:
      return QCxml_error_xptr_resource_error;
    case XML_XPTR_SUB_RESOURCE_ERROR:
      return QCxml_error_xptr_sub_resource_error;
    case XML_XPATH_UNDEF_PREFIX_ERROR:
      return QCxml_error_xpath_undef_prefix_error;
    case XML_XPATH_ENCODING_ERROR:
      return QCxml_error_xpath_encoding_error;
    case XML_XPATH_INVALID_CHAR_ERROR:
      return QCxml_error_xpath_invalid_char_error;
    case XML_TREE_INVALID_HEX:
      return QCxml_error_tree_invalid_hex;
    case XML_TREE_INVALID_DEC:
      return QCxml_error_tree_invalid_dec;
    case XML_TREE_UNTERMINATED_ENTITY:
      return QCxml_error_tree_unterminated_entity;
    case XML_TREE_NOT_UTF8:
      return QCxml_error_tree_not_utf8;
    case XML_SAVE_NOT_UTF8:
      return QCxml_error_save_not_utf8;
    case XML_SAVE_CHAR_INVALID:
      return QCxml_error_save_char_invalid;
    case XML_SAVE_NO_DOCTYPE:
      return QCxml_error_save_no_doctype;
    case XML_SAVE_UNKNOWN_ENCODING:
      return QCxml_error_save_unknown_encoding;
    case XML_REGEXP_COMPILE_ERROR:
      return QCxml_error_regexp_compile_error;
    case XML_IO_UNKNOWN:
      return QCxml_error_io_unknown;
    case XML_IO_EACCES:
      return QCxml_error_io_eacces;
    case XML_IO_EAGAIN:
      return QCxml_error_io_eagain;
    case XML_IO_EBADF:
      return QCxml_error_io_ebadf;
    case XML_IO_EBADMSG:
      return QCxml_error_io_ebadmsg;
    case XML_IO_EBUSY:
      return QCxml_error_io_ebusy;
    case XML_IO_ECANCELED:
      return QCxml_error_io_ecanceled;
    case XML_IO_ECHILD:
      return QCxml_error_io_echild;
    case XML_IO_EDEADLK:
      return QCxml_error_io_edeadlk;
    case XML_IO_EDOM:
      return QCxml_error_io_edom;
    case XML_IO_EEXIST:
      return QCxml_error_io_eexist;
    case XML_IO_EFAULT:
      return QCxml_error_io_efault;
    case XML_IO_EFBIG:
      return QCxml_error_io_efbig;
    case XML_IO_EINPROGRESS:
      return QCxml_error_io_einprogress;
    case XML_IO_EINTR:
      return QCxml_error_io_eintr;
    case XML_IO_EINVAL:
      return QCxml_error_io_einval;
    case XML_IO_EIO:
      return QCxml_error_io_eio;
    case XML_IO_EISDIR:
      return QCxml_error_io_eisdir;
    case XML_IO_EMFILE:
      return QCxml_error_io_emfile;
    case XML_IO_EMLINK:
      return QCxml_error_io_emlink;
    case XML_IO_EMSGSIZE:
      return QCxml_error_io_emsgsize;
    case XML_IO_ENAMETOOLONG:
      return QCxml_error_io_enametoolong;
    case XML_IO_ENFILE:
      return QCxml_error_io_enfile;
    case XML_IO_ENODEV:
      return QCxml_error_io_enodev;
    case XML_IO_ENOENT:
      return QCxml_error_io_enoent;
    case XML_IO_ENOEXEC:
      return QCxml_error_io_enoexec;
    case XML_IO_ENOLCK:
      return QCxml_error_io_enolck;
    case XML_IO_ENOMEM:
      return QCxml_error_io_enomem;
    case XML_IO_ENOSPC:
      return QCxml_error_io_enospc;
    case XML_IO_ENOSYS:
      return QCxml_error_io_enosys;
    case XML_IO_ENOTDIR:
      return QCxml_error_io_enotdir;
    case XML_IO_ENOTEMPTY:
      return QCxml_error_io_enotempty;
    case XML_IO_ENOTSUP:
      return QCxml_error_io_enotsup;
    case XML_IO_ENOTTY:
      return QCxml_error_io_enotty;
    case XML_IO_ENXIO:
      return QCxml_error_io_enxio;
    case XML_IO_EPERM:
      return QCxml_error_io_eperm;
    case XML_IO_EPIPE:
      return QCxml_error_io_epipe;
    case XML_IO_ERANGE:
      return QCxml_error_io_erange;
    case XML_IO_EROFS:
      return QCxml_error_io_erofs;
    case XML_IO_ESPIPE:
      return QCxml_error_io_espipe;
    case XML_IO_ESRCH:
      return QCxml_error_io_esrch;
    case XML_IO_ETIMEDOUT:
      return QCxml_error_io_etimedout;
    case XML_IO_EXDEV:
      return QCxml_error_io_exdev;
    case XML_IO_NETWORK_ATTEMPT:
      return QCxml_error_io_network_attempt;
    case XML_IO_ENCODER:
      return QCxml_error_io_encoder;
    case XML_IO_FLUSH:
      return QCxml_error_io_flush;
    case XML_IO_WRITE:
      return QCxml_error_io_write;
    case XML_IO_NO_INPUT:
      return QCxml_error_io_no_input;
    case XML_IO_BUFFER_FULL:
      return QCxml_error_io_buffer_full;
    case XML_IO_LOAD_ERROR:
      return QCxml_error_io_load_error;
    case XML_IO_ENOTSOCK:
      return QCxml_error_io_enotsock;
    case XML_IO_EISCONN:
      return QCxml_error_io_eisconn;
    case XML_IO_ECONNREFUSED:
      return QCxml_error_io_econnrefused;
    case XML_IO_ENETUNREACH:
      return QCxml_error_io_enetunreach;
    case XML_IO_EADDRINUSE:
      return QCxml_error_io_eaddrinuse;
    case XML_IO_EALREADY:
      return QCxml_error_io_ealready;
    case XML_IO_EAFNOSUPPORT:
      return QCxml_error_io_eafnosupport;
    case XML_XINCLUDE_RECURSION:
      return QCxml_error_xinclude_recursion;
    case XML_XINCLUDE_PARSE_VALUE:
      return QCxml_error_xinclude_parse_value;
    case XML_XINCLUDE_ENTITY_DEF_MISMATCH:
      return QCxml_error_xinclude_entity_def_mismatch;
    case XML_XINCLUDE_NO_HREF:
      return QCxml_error_xinclude_no_href;
    case XML_XINCLUDE_NO_FALLBACK:
      return QCxml_error_xinclude_no_fallback;
    case XML_XINCLUDE_HREF_URI:
      return QCxml_error_xinclude_href_uri;
    case XML_XINCLUDE_TEXT_FRAGMENT:
      return QCxml_error_xinclude_text_fragment;
    case XML_XINCLUDE_TEXT_DOCUMENT:
      return QCxml_error_xinclude_text_document;
    case XML_XINCLUDE_INVALID_CHAR:
      return QCxml_error_xinclude_invalid_char;
    case XML_XINCLUDE_BUILD_FAILED:
      return QCxml_error_xinclude_build_failed;
    case XML_XINCLUDE_UNKNOWN_ENCODING:
      return QCxml_error_xinclude_unknown_encoding;
    case XML_XINCLUDE_MULTIPLE_ROOT:
      return QCxml_error_xinclude_multiple_root;
    case XML_XINCLUDE_XPTR_FAILED:
      return QCxml_error_xinclude_xptr_failed;
    case XML_XINCLUDE_XPTR_RESULT:
      return QCxml_error_xinclude_xptr_result;
    case XML_XINCLUDE_INCLUDE_IN_INCLUDE:
      return QCxml_error_xinclude_include_in_include;
    case XML_XINCLUDE_FALLBACKS_IN_INCLUDE:
      return QCxml_error_xinclude_fallbacks_in_include;
    case XML_XINCLUDE_FALLBACK_NOT_IN_INCLUDE:
      return QCxml_error_xinclude_fallback_not_in_include;
    case XML_XINCLUDE_DEPRECATED_NS:
      return QCxml_error_xinclude_deprecated_ns;
    case XML_XINCLUDE_FRAGMENT_ID:
      return QCxml_error_xinclude_fragment_id;
    case XML_CATALOG_MISSING_ATTR:
      return QCxml_error_catalog_missing_attr;
    case XML_CATALOG_ENTRY_BROKEN:
      return QCxml_error_catalog_entry_broken;
    case XML_CATALOG_PREFER_VALUE:
      return QCxml_error_catalog_prefer_value;
    case XML_CATALOG_NOT_CATALOG:
      return QCxml_error_catalog_not_catalog;
    case XML_CATALOG_RECURSION:
      return QCxml_error_catalog_recursion;
    case XML_SCHEMAP_PREFIX_UNDEFINED:
      return QCxml_error_schemap_prefix_undefined;
    case XML_SCHEMAP_ATTRFORMDEFAULT_VALUE:
      return QCxml_error_schemap_attrformdefault_value;
    case XML_SCHEMAP_ATTRGRP_NONAME_NOREF:
      return QCxml_error_schemap_attrgrp_noname_noref;
    case XML_SCHEMAP_ATTR_NONAME_NOREF:
      return QCxml_error_schemap_attr_noname_noref;
    case XML_SCHEMAP_COMPLEXTYPE_NONAME_NOREF:
      return QCxml_error_schemap_complextype_noname_noref;
    case XML_SCHEMAP_ELEMFORMDEFAULT_VALUE:
      return QCxml_error_schemap_elemformdefault_value;
    case XML_SCHEMAP_ELEM_NONAME_NOREF:
      return QCxml_error_schemap_elem_noname_noref;
    case XML_SCHEMAP_EXTENSION_NO_BASE:
      return QCxml_error_schemap_extension_no_base;
    case XML_SCHEMAP_FACET_NO_VALUE:
      return QCxml_error_schemap_facet_no_value;
    case XML_SCHEMAP_FAILED_BUILD_IMPORT:
      return QCxml_error_schemap_failed_build_import;
    case XML_SCHEMAP_GROUP_NONAME_NOREF:
      return QCxml_error_schemap_group_noname_noref;
    case XML_SCHEMAP_IMPORT_NAMESPACE_NOT_URI:
      return QCxml_error_schemap_import_namespace_not_uri;
    case XML_SCHEMAP_IMPORT_REDEFINE_NSNAME:
      return QCxml_error_schemap_import_redefine_nsname;
    case XML_SCHEMAP_IMPORT_SCHEMA_NOT_URI:
      return QCxml_error_schemap_import_schema_not_uri;
    case XML_SCHEMAP_INVALID_BOOLEAN:
      return QCxml_error_schemap_invalid_boolean;
    case XML_SCHEMAP_INVALID_ENUM:
      return QCxml_error_schemap_invalid_enum;
    case XML_SCHEMAP_INVALID_FACET:
      return QCxml_error_schemap_invalid_facet;
    case XML_SCHEMAP_INVALID_FACET_VALUE:
      return QCxml_error_schemap_invalid_facet_value;
    case XML_SCHEMAP_INVALID_MAXOCCURS:
      return QCxml_error_schemap_invalid_maxoccurs;
    case XML_SCHEMAP_INVALID_MINOCCURS:
      return QCxml_error_schemap_invalid_minoccurs;
    case XML_SCHEMAP_INVALID_REF_AND_SUBTYPE:
      return QCxml_error_schemap_invalid_ref_and_subtype;
    case XML_SCHEMAP_INVALID_WHITE_SPACE:
      return QCxml_error_schemap_invalid_white_space;
    case XML_SCHEMAP_NOATTR_NOREF:
      return QCxml_error_schemap_noattr_noref;
    case XML_SCHEMAP_NOTATION_NO_NAME:
      return QCxml_error_schemap_notation_no_name;
    case XML_SCHEMAP_NOTYPE_NOREF:
      return QCxml_error_schemap_notype_noref;
    case XML_SCHEMAP_REF_AND_SUBTYPE:
      return QCxml_error_schemap_ref_and_subtype;
    case XML_SCHEMAP_RESTRICTION_NONAME_NOREF:
      return QCxml_error_schemap_restriction_noname_noref;
    case XML_SCHEMAP_SIMPLETYPE_NONAME:
      return QCxml_error_schemap_simpletype_noname;
    case XML_SCHEMAP_TYPE_AND_SUBTYPE:
      return QCxml_error_schemap_type_and_subtype;
    case XML_SCHEMAP_UNKNOWN_ALL_CHILD:
      return QCxml_error_schemap_unknown_all_child;
    case XML_SCHEMAP_UNKNOWN_ANYATTRIBUTE_CHILD:
      return QCxml_error_schemap_unknown_anyattribute_child;
    case XML_SCHEMAP_UNKNOWN_ATTR_CHILD:
      return QCxml_error_schemap_unknown_attr_child;
    case XML_SCHEMAP_UNKNOWN_ATTRGRP_CHILD:
      return QCxml_error_schemap_unknown_attrgrp_child;
    case XML_SCHEMAP_UNKNOWN_ATTRIBUTE_GROUP:
      return QCxml_error_schemap_unknown_attribute_group;
    case XML_SCHEMAP_UNKNOWN_BASE_TYPE:
      return QCxml_error_schemap_unknown_base_type;
    case XML_SCHEMAP_UNKNOWN_CHOICE_CHILD:
      return QCxml_error_schemap_unknown_choice_child;
    case XML_SCHEMAP_UNKNOWN_COMPLEXCONTENT_CHILD:
      return QCxml_error_schemap_unknown_complexcontent_child;
    case XML_SCHEMAP_UNKNOWN_COMPLEXTYPE_CHILD:
      return QCxml_error_schemap_unknown_complextype_child;
    case XML_SCHEMAP_UNKNOWN_ELEM_CHILD:
      return QCxml_error_schemap_unknown_elem_child;
    case XML_SCHEMAP_UNKNOWN_EXTENSION_CHILD:
      return QCxml_error_schemap_unknown_extension_child;
    case XML_SCHEMAP_UNKNOWN_FACET_CHILD:
      return QCxml_error_schemap_unknown_facet_child;
    case XML_SCHEMAP_UNKNOWN_FACET_TYPE:
      return QCxml_error_schemap_unknown_facet_type;
    case XML_SCHEMAP_UNKNOWN_GROUP_CHILD:
      return QCxml_error_schemap_unknown_group_child;
    case XML_SCHEMAP_UNKNOWN_IMPORT_CHILD:
      return QCxml_error_schemap_unknown_import_child;
    case XML_SCHEMAP_UNKNOWN_LIST_CHILD:
      return QCxml_error_schemap_unknown_list_child;
    case XML_SCHEMAP_UNKNOWN_NOTATION_CHILD:
      return QCxml_error_schemap_unknown_notation_child;
    case XML_SCHEMAP_UNKNOWN_PROCESSCONTENT_CHILD:
      return QCxml_error_schemap_unknown_processcontent_child;
    case XML_SCHEMAP_UNKNOWN_REF:
      return QCxml_error_schemap_unknown_ref;
    case XML_SCHEMAP_UNKNOWN_RESTRICTION_CHILD:
      return QCxml_error_schemap_unknown_restriction_child;
    case XML_SCHEMAP_UNKNOWN_SCHEMAS_CHILD:
      return QCxml_error_schemap_unknown_schemas_child;
    case XML_SCHEMAP_UNKNOWN_SEQUENCE_CHILD:
      return QCxml_error_schemap_unknown_sequence_child;
    case XML_SCHEMAP_UNKNOWN_SIMPLECONTENT_CHILD:
      return QCxml_error_schemap_unknown_simplecontent_child;
    case XML_SCHEMAP_UNKNOWN_SIMPLETYPE_CHILD:
      return QCxml_error_schemap_unknown_simpletype_child;
    case XML_SCHEMAP_UNKNOWN_TYPE:
      return QCxml_error_schemap_unknown_type;
    case XML_SCHEMAP_UNKNOWN_UNION_CHILD:
      return QCxml_error_schemap_unknown_union_child;
    case XML_SCHEMAP_ELEM_DEFAULT_FIXED:
      return QCxml_error_schemap_elem_default_fixed;
    case XML_SCHEMAP_REGEXP_INVALID:
      return QCxml_error_schemap_regexp_invalid;
    case XML_SCHEMAP_FAILED_LOAD:
      return QCxml_error_schemap_failed_load;
    case XML_SCHEMAP_NOTHING_TO_PARSE:
      return QCxml_error_schemap_nothing_to_parse;
    case XML_SCHEMAP_NOROOT:
      return QCxml_error_schemap_noroot;
    case XML_SCHEMAP_REDEFINED_GROUP:
      return QCxml_error_schemap_redefined_group;
    case XML_SCHEMAP_REDEFINED_TYPE:
      return QCxml_error_schemap_redefined_type;
    case XML_SCHEMAP_REDEFINED_ELEMENT:
      return QCxml_error_schemap_redefined_element;
    case XML_SCHEMAP_REDEFINED_ATTRGROUP:
      return QCxml_error_schemap_redefined_attrgroup;
    case XML_SCHEMAP_REDEFINED_ATTR:
      return QCxml_error_schemap_redefined_attr;
    case XML_SCHEMAP_REDEFINED_NOTATION:
      return QCxml_error_schemap_redefined_notation;
    case XML_SCHEMAP_FAILED_PARSE:
      return QCxml_error_schemap_failed_parse;
    case XML_SCHEMAP_UNKNOWN_PREFIX:
      return QCxml_error_schemap_unknown_prefix;
    case XML_SCHEMAP_DEF_AND_PREFIX:
      return QCxml_error_schemap_def_and_prefix;
    case XML_SCHEMAP_UNKNOWN_INCLUDE_CHILD:
      return QCxml_error_schemap_unknown_include_child;
    case XML_SCHEMAP_INCLUDE_SCHEMA_NOT_URI:
      return QCxml_error_schemap_include_schema_not_uri;
    case XML_SCHEMAP_INCLUDE_SCHEMA_NO_URI:
      return QCxml_error_schemap_include_schema_no_uri;
    case XML_SCHEMAP_NOT_SCHEMA:
      return QCxml_error_schemap_not_schema;
    case XML_SCHEMAP_UNKNOWN_MEMBER_TYPE:
      return QCxml_error_schemap_unknown_member_type;
    case XML_SCHEMAP_INVALID_ATTR_USE:
      return QCxml_error_schemap_invalid_attr_use;
    case XML_SCHEMAP_RECURSIVE:
      return QCxml_error_schemap_recursive;
    case XML_SCHEMAP_SUPERNUMEROUS_LIST_ITEM_TYPE:
      return QCxml_error_schemap_supernumerous_list_item_type;
    case XML_SCHEMAP_INVALID_ATTR_COMBINATION:
      return QCxml_error_schemap_invalid_attr_combination;
    case XML_SCHEMAP_INVALID_ATTR_INLINE_COMBINATION:
      return QCxml_error_schemap_invalid_attr_inline_combination;
    case XML_SCHEMAP_MISSING_SIMPLETYPE_CHILD:
      return QCxml_error_schemap_missing_simpletype_child;
    case XML_SCHEMAP_INVALID_ATTR_NAME:
      return QCxml_error_schemap_invalid_attr_name;
    case XML_SCHEMAP_REF_AND_CONTENT:
      return QCxml_error_schemap_ref_and_content;
    case XML_SCHEMAP_CT_PROPS_CORRECT_1:
      return QCxml_error_schemap_ct_props_correct_1;
    case XML_SCHEMAP_CT_PROPS_CORRECT_2:
      return QCxml_error_schemap_ct_props_correct_2;
    case XML_SCHEMAP_CT_PROPS_CORRECT_3:
      return QCxml_error_schemap_ct_props_correct_3;
    case XML_SCHEMAP_CT_PROPS_CORRECT_4:
      return QCxml_error_schemap_ct_props_correct_4;
    case XML_SCHEMAP_CT_PROPS_CORRECT_5:
      return QCxml_error_schemap_ct_props_correct_5;
    case XML_SCHEMAP_DERIVATION_OK_RESTRICTION_1:
      return QCxml_error_schemap_derivation_ok_restriction_1;
    case XML_SCHEMAP_DERIVATION_OK_RESTRICTION_2_1_1:
      return QCxml_error_schemap_derivation_ok_restriction_2_1_1;
    case XML_SCHEMAP_DERIVATION_OK_RESTRICTION_2_1_2:
      return QCxml_error_schemap_derivation_ok_restriction_2_1_2;
    case XML_SCHEMAP_DERIVATION_OK_RESTRICTION_2_2:
      return QCxml_error_schemap_derivation_ok_restriction_2_2;
    case XML_SCHEMAP_DERIVATION_OK_RESTRICTION_3:
      return QCxml_error_schemap_derivation_ok_restriction_3;
    case XML_SCHEMAP_WILDCARD_INVALID_NS_MEMBER:
      return QCxml_error_schemap_wildcard_invalid_ns_member;
    case XML_SCHEMAP_INTERSECTION_NOT_EXPRESSIBLE:
      return QCxml_error_schemap_intersection_not_expressible;
    case XML_SCHEMAP_UNION_NOT_EXPRESSIBLE:
      return QCxml_error_schemap_union_not_expressible;
    case XML_SCHEMAP_SRC_IMPORT_3_1:
      return QCxml_error_schemap_src_import_3_1;
    case XML_SCHEMAP_SRC_IMPORT_3_2:
      return QCxml_error_schemap_src_import_3_2;
    case XML_SCHEMAP_DERIVATION_OK_RESTRICTION_4_1:
      return QCxml_error_schemap_derivation_ok_restriction_4_1;
    case XML_SCHEMAP_DERIVATION_OK_RESTRICTION_4_2:
      return QCxml_error_schemap_derivation_ok_restriction_4_2;
    case XML_SCHEMAP_DERIVATION_OK_RESTRICTION_4_3:
      return QCxml_error_schemap_derivation_ok_restriction_4_3;
    case XML_SCHEMAP_COS_CT_EXTENDS_1_3:
      return QCxml_error_schemap_cos_ct_extends_1_3;
    case XML_SCHEMAV_NOROOT:
      return QCxml_error_schemav_noroot;
    case XML_SCHEMAV_UNDECLAREDELEM:
      return QCxml_error_schemav_undeclaredelem;
    case XML_SCHEMAV_NOTTOPLEVEL:
      return QCxml_error_schemav_nottoplevel;
    case XML_SCHEMAV_MISSING:
      return QCxml_error_schemav_missing;
    case XML_SCHEMAV_WRONGELEM:
      return QCxml_error_schemav_wrongelem;
    case XML_SCHEMAV_NOTYPE:
      return QCxml_error_schemav_notype;
    case XML_SCHEMAV_NOROLLBACK:
      return QCxml_error_schemav_norollback;
    case XML_SCHEMAV_ISABSTRACT:
      return QCxml_error_schemav_isabstract;
    case XML_SCHEMAV_NOTEMPTY:
      return QCxml_error_schemav_notempty;
    case XML_SCHEMAV_ELEMCONT:
      return QCxml_error_schemav_elemcont;
    case XML_SCHEMAV_HAVEDEFAULT:
      return QCxml_error_schemav_havedefault;
    case XML_SCHEMAV_NOTNILLABLE:
      return QCxml_error_schemav_notnillable;
    case XML_SCHEMAV_EXTRACONTENT:
      return QCxml_error_schemav_extracontent;
    case XML_SCHEMAV_INVALIDATTR:
      return QCxml_error_schemav_invalidattr;
    case XML_SCHEMAV_INVALIDELEM:
      return QCxml_error_schemav_invalidelem;
    case XML_SCHEMAV_NOTDETERMINIST:
      return QCxml_error_schemav_notdeterminist;
    case XML_SCHEMAV_CONSTRUCT:
      return QCxml_error_schemav_construct;
    case XML_SCHEMAV_INTERNAL:
      return QCxml_error_schemav_internal;
    case XML_SCHEMAV_NOTSIMPLE:
      return QCxml_error_schemav_notsimple;
    case XML_SCHEMAV_ATTRUNKNOWN:
      return QCxml_error_schemav_attrunknown;
    case XML_SCHEMAV_ATTRINVALID:
      return QCxml_error_schemav_attrinvalid;
    case XML_SCHEMAV_VALUE:
      return QCxml_error_schemav_value;
    case XML_SCHEMAV_FACET:
      return QCxml_error_schemav_facet;
    case XML_SCHEMAV_CVC_DATATYPE_VALID_1_2_1:
      return QCxml_error_schemav_cvc_datatype_valid_1_2_1;
    case XML_SCHEMAV_CVC_DATATYPE_VALID_1_2_2:
      return QCxml_error_schemav_cvc_datatype_valid_1_2_2;
    case XML_SCHEMAV_CVC_DATATYPE_VALID_1_2_3:
      return QCxml_error_schemav_cvc_datatype_valid_1_2_3;
    case XML_SCHEMAV_CVC_TYPE_3_1_1:
      return QCxml_error_schemav_cvc_type_3_1_1;
    case XML_SCHEMAV_CVC_TYPE_3_1_2:
      return QCxml_error_schemav_cvc_type_3_1_2;
    case XML_SCHEMAV_CVC_FACET_VALID:
      return QCxml_error_schemav_cvc_facet_valid;
    case XML_SCHEMAV_CVC_LENGTH_VALID:
      return QCxml_error_schemav_cvc_length_valid;
    case XML_SCHEMAV_CVC_MINLENGTH_VALID:
      return QCxml_error_schemav_cvc_minlength_valid;
    case XML_SCHEMAV_CVC_MAXLENGTH_VALID:
      return QCxml_error_schemav_cvc_maxlength_valid;
    case XML_SCHEMAV_CVC_MININCLUSIVE_VALID:
      return QCxml_error_schemav_cvc_mininclusive_valid;
    case XML_SCHEMAV_CVC_MAXINCLUSIVE_VALID:
      return QCxml_error_schemav_cvc_maxinclusive_valid;
    case XML_SCHEMAV_CVC_MINEXCLUSIVE_VALID:
      return QCxml_error_schemav_cvc_minexclusive_valid;
    case XML_SCHEMAV_CVC_MAXEXCLUSIVE_VALID:
      return QCxml_error_schemav_cvc_maxexclusive_valid;
    case XML_SCHEMAV_CVC_TOTALDIGITS_VALID:
      return QCxml_error_schemav_cvc_totaldigits_valid;
    case XML_SCHEMAV_CVC_FRACTIONDIGITS_VALID:
      return QCxml_error_schemav_cvc_fractiondigits_valid;
    case XML_SCHEMAV_CVC_PATTERN_VALID:
      return QCxml_error_schemav_cvc_pattern_valid;
    case XML_SCHEMAV_CVC_ENUMERATION_VALID:
      return QCxml_error_schemav_cvc_enumeration_valid;
    case XML_SCHEMAV_CVC_COMPLEX_TYPE_2_1:
      return QCxml_error_schemav_cvc_complex_type_2_1;
    case XML_SCHEMAV_CVC_COMPLEX_TYPE_2_2:
      return QCxml_error_schemav_cvc_complex_type_2_2;
    case XML_SCHEMAV_CVC_COMPLEX_TYPE_2_3:
      return QCxml_error_schemav_cvc_complex_type_2_3;
    case XML_SCHEMAV_CVC_COMPLEX_TYPE_2_4:
      return QCxml_error_schemav_cvc_complex_type_2_4;
    case XML_SCHEMAV_CVC_ELT_1:
      return QCxml_error_schemav_cvc_elt_1;
    case XML_SCHEMAV_CVC_ELT_2:
      return QCxml_error_schemav_cvc_elt_2;
    case XML_SCHEMAV_CVC_ELT_3_1:
      return QCxml_error_schemav_cvc_elt_3_1;
    case XML_SCHEMAV_CVC_ELT_3_2_1:
      return QCxml_error_schemav_cvc_elt_3_2_1;
    case XML_SCHEMAV_CVC_ELT_3_2_2:
      return QCxml_error_schemav_cvc_elt_3_2_2;
    case XML_SCHEMAV_CVC_ELT_4_1:
      return QCxml_error_schemav_cvc_elt_4_1;
    case XML_SCHEMAV_CVC_ELT_4_2:
      return QCxml_error_schemav_cvc_elt_4_2;
    case XML_SCHEMAV_CVC_ELT_4_3:
      return QCxml_error_schemav_cvc_elt_4_3;
    case XML_SCHEMAV_CVC_ELT_5_1_1:
      return QCxml_error_schemav_cvc_elt_5_1_1;
    case XML_SCHEMAV_CVC_ELT_5_1_2:
      return QCxml_error_schemav_cvc_elt_5_1_2;
    case XML_SCHEMAV_CVC_ELT_5_2_1:
      return QCxml_error_schemav_cvc_elt_5_2_1;
    case XML_SCHEMAV_CVC_ELT_5_2_2_1:
      return QCxml_error_schemav_cvc_elt_5_2_2_1;
    case XML_SCHEMAV_CVC_ELT_5_2_2_2_1:
      return QCxml_error_schemav_cvc_elt_5_2_2_2_1;
    case XML_SCHEMAV_CVC_ELT_5_2_2_2_2:
      return QCxml_error_schemav_cvc_elt_5_2_2_2_2;
    case XML_SCHEMAV_CVC_ELT_6:
      return QCxml_error_schemav_cvc_elt_6;
    case XML_SCHEMAV_CVC_ELT_7:
      return QCxml_error_schemav_cvc_elt_7;
    case XML_SCHEMAV_CVC_ATTRIBUTE_1:
      return QCxml_error_schemav_cvc_attribute_1;
    case XML_SCHEMAV_CVC_ATTRIBUTE_2:
      return QCxml_error_schemav_cvc_attribute_2;
    case XML_SCHEMAV_CVC_ATTRIBUTE_3:
      return QCxml_error_schemav_cvc_attribute_3;
    case XML_SCHEMAV_CVC_ATTRIBUTE_4:
      return QCxml_error_schemav_cvc_attribute_4;
    case XML_SCHEMAV_CVC_COMPLEX_TYPE_3_1:
      return QCxml_error_schemav_cvc_complex_type_3_1;
    case XML_SCHEMAV_CVC_COMPLEX_TYPE_3_2_1:
      return QCxml_error_schemav_cvc_complex_type_3_2_1;
    case XML_SCHEMAV_CVC_COMPLEX_TYPE_3_2_2:
      return QCxml_error_schemav_cvc_complex_type_3_2_2;
    case XML_SCHEMAV_CVC_COMPLEX_TYPE_4:
      return QCxml_error_schemav_cvc_complex_type_4;
    case XML_SCHEMAV_CVC_COMPLEX_TYPE_5_1:
      return QCxml_error_schemav_cvc_complex_type_5_1;
    case XML_SCHEMAV_CVC_COMPLEX_TYPE_5_2:
      return QCxml_error_schemav_cvc_complex_type_5_2;
    case XML_SCHEMAV_ELEMENT_CONTENT:
      return QCxml_error_schemav_element_content;
    case XML_SCHEMAV_DOCUMENT_ELEMENT_MISSING:
      return QCxml_error_schemav_document_element_missing;
    case XML_SCHEMAV_CVC_COMPLEX_TYPE_1:
      return QCxml_error_schemav_cvc_complex_type_1;
    case XML_SCHEMAV_CVC_AU:
      return QCxml_error_schemav_cvc_au;
    case XML_SCHEMAV_CVC_TYPE_1:
      return QCxml_error_schemav_cvc_type_1;
    case XML_SCHEMAV_CVC_TYPE_2:
      return QCxml_error_schemav_cvc_type_2;
    case XML_SCHEMAV_CVC_IDC:
      return QCxml_error_schemav_cvc_idc;
    case XML_SCHEMAV_CVC_WILDCARD:
      return QCxml_error_schemav_cvc_wildcard;
    case XML_SCHEMAV_MISC:
      return QCxml_error_schemav_misc;
    case XML_XPTR_UNKNOWN_SCHEME:
      return QCxml_error_xptr_unknown_scheme;
    case XML_XPTR_CHILDSEQ_START:
      return QCxml_error_xptr_childseq_start;
    case XML_XPTR_EVAL_FAILED:
      return QCxml_error_xptr_eval_failed;
    case XML_XPTR_EXTRA_OBJECTS:
      return QCxml_error_xptr_extra_objects;
    case XML_C14N_CREATE_CTXT:
      return QCxml_error_c14n_create_ctxt;
    case XML_C14N_REQUIRES_UTF8:
      return QCxml_error_c14n_requires_utf8;
    case XML_C14N_CREATE_STACK:
      return QCxml_error_c14n_create_stack;
    case XML_C14N_INVALID_NODE:
      return QCxml_error_c14n_invalid_node;
    case XML_C14N_UNKNOW_NODE:
      return QCxml_error_c14n_unknow_node;
    case XML_C14N_RELATIVE_NAMESPACE:
      return QCxml_error_c14n_relative_namespace;
    case XML_FTP_PASV_ANSWER:
      return QCxml_error_ftp_pasv_answer;
    case XML_FTP_EPSV_ANSWER:
      return QCxml_error_ftp_epsv_answer;
    case XML_FTP_ACCNT:
      return QCxml_error_ftp_accnt;
    case XML_FTP_URL_SYNTAX:
      return QCxml_error_ftp_url_syntax;
    case XML_HTTP_URL_SYNTAX:
      return QCxml_error_http_url_syntax;
    case XML_HTTP_USE_IP:
      return QCxml_error_http_use_ip;
    case XML_HTTP_UNKNOWN_HOST:
      return QCxml_error_http_unknown_host;
    case XML_SCHEMAP_SRC_SIMPLE_TYPE_1:
      return QCxml_error_schemap_src_simple_type_1;
    case XML_SCHEMAP_SRC_SIMPLE_TYPE_2:
      return QCxml_error_schemap_src_simple_type_2;
    case XML_SCHEMAP_SRC_SIMPLE_TYPE_3:
      return QCxml_error_schemap_src_simple_type_3;
    case XML_SCHEMAP_SRC_SIMPLE_TYPE_4:
      return QCxml_error_schemap_src_simple_type_4;
    case XML_SCHEMAP_SRC_RESOLVE:
      return QCxml_error_schemap_src_resolve;
    case XML_SCHEMAP_SRC_RESTRICTION_BASE_OR_SIMPLETYPE:
      return QCxml_error_schemap_src_restriction_base_or_simpletype;
    case XML_SCHEMAP_SRC_LIST_ITEMTYPE_OR_SIMPLETYPE:
      return QCxml_error_schemap_src_list_itemtype_or_simpletype;
    case XML_SCHEMAP_SRC_UNION_MEMBERTYPES_OR_SIMPLETYPES:
      return QCxml_error_schemap_src_union_membertypes_or_simpletypes;
    case XML_SCHEMAP_ST_PROPS_CORRECT_1:
      return QCxml_error_schemap_st_props_correct_1;
    case XML_SCHEMAP_ST_PROPS_CORRECT_2:
      return QCxml_error_schemap_st_props_correct_2;
    case XML_SCHEMAP_ST_PROPS_CORRECT_3:
      return QCxml_error_schemap_st_props_correct_3;
    case XML_SCHEMAP_COS_ST_RESTRICTS_1_1:
      return QCxml_error_schemap_cos_st_restricts_1_1;
    case XML_SCHEMAP_COS_ST_RESTRICTS_1_2:
      return QCxml_error_schemap_cos_st_restricts_1_2;
    case XML_SCHEMAP_COS_ST_RESTRICTS_1_3_1:
      return QCxml_error_schemap_cos_st_restricts_1_3_1;
    case XML_SCHEMAP_COS_ST_RESTRICTS_1_3_2:
      return QCxml_error_schemap_cos_st_restricts_1_3_2;
    case XML_SCHEMAP_COS_ST_RESTRICTS_2_1:
      return QCxml_error_schemap_cos_st_restricts_2_1;
    case XML_SCHEMAP_COS_ST_RESTRICTS_2_3_1_1:
      return QCxml_error_schemap_cos_st_restricts_2_3_1_1;
    case XML_SCHEMAP_COS_ST_RESTRICTS_2_3_1_2:
      return QCxml_error_schemap_cos_st_restricts_2_3_1_2;
    case XML_SCHEMAP_COS_ST_RESTRICTS_2_3_2_1:
      return QCxml_error_schemap_cos_st_restricts_2_3_2_1;
    case XML_SCHEMAP_COS_ST_RESTRICTS_2_3_2_2:
      return QCxml_error_schemap_cos_st_restricts_2_3_2_2;
    case XML_SCHEMAP_COS_ST_RESTRICTS_2_3_2_3:
      return QCxml_error_schemap_cos_st_restricts_2_3_2_3;
    case XML_SCHEMAP_COS_ST_RESTRICTS_2_3_2_4:
      return QCxml_error_schemap_cos_st_restricts_2_3_2_4;
    case XML_SCHEMAP_COS_ST_RESTRICTS_2_3_2_5:
      return QCxml_error_schemap_cos_st_restricts_2_3_2_5;
    case XML_SCHEMAP_COS_ST_RESTRICTS_3_1:
      return QCxml_error_schemap_cos_st_restricts_3_1;
    case XML_SCHEMAP_COS_ST_RESTRICTS_3_3_1:
      return QCxml_error_schemap_cos_st_restricts_3_3_1;
    case XML_SCHEMAP_COS_ST_RESTRICTS_3_3_1_2:
      return QCxml_error_schemap_cos_st_restricts_3_3_1_2;
    case XML_SCHEMAP_COS_ST_RESTRICTS_3_3_2_2:
      return QCxml_error_schemap_cos_st_restricts_3_3_2_2;
    case XML_SCHEMAP_COS_ST_RESTRICTS_3_3_2_1:
      return QCxml_error_schemap_cos_st_restricts_3_3_2_1;
    case XML_SCHEMAP_COS_ST_RESTRICTS_3_3_2_3:
      return QCxml_error_schemap_cos_st_restricts_3_3_2_3;
    case XML_SCHEMAP_COS_ST_RESTRICTS_3_3_2_4:
      return QCxml_error_schemap_cos_st_restricts_3_3_2_4;
    case XML_SCHEMAP_COS_ST_RESTRICTS_3_3_2_5:
      return QCxml_error_schemap_cos_st_restricts_3_3_2_5;
    case XML_SCHEMAP_COS_ST_DERIVED_OK_2_1:
      return QCxml_error_schemap_cos_st_derived_ok_2_1;
    case XML_SCHEMAP_COS_ST_DERIVED_OK_2_2:
      return QCxml_error_schemap_cos_st_derived_ok_2_2;
    case XML_SCHEMAP_S4S_ELEM_NOT_ALLOWED:
      return QCxml_error_schemap_s4s_elem_not_allowed;
    case XML_SCHEMAP_S4S_ELEM_MISSING:
      return QCxml_error_schemap_s4s_elem_missing;
    case XML_SCHEMAP_S4S_ATTR_NOT_ALLOWED:
      return QCxml_error_schemap_s4s_attr_not_allowed;
    case XML_SCHEMAP_S4S_ATTR_MISSING:
      return QCxml_error_schemap_s4s_attr_missing;
    case XML_SCHEMAP_S4S_ATTR_INVALID_VALUE:
      return QCxml_error_schemap_s4s_attr_invalid_value;
    case XML_SCHEMAP_SRC_ELEMENT_1:
      return QCxml_error_schemap_src_element_1;
    case XML_SCHEMAP_SRC_ELEMENT_2_1:
      return QCxml_error_schemap_src_element_2_1;
    case XML_SCHEMAP_SRC_ELEMENT_2_2:
      return QCxml_error_schemap_src_element_2_2;
    case XML_SCHEMAP_SRC_ELEMENT_3:
      return QCxml_error_schemap_src_element_3;
    case XML_SCHEMAP_P_PROPS_CORRECT_1:
      return QCxml_error_schemap_p_props_correct_1;
    case XML_SCHEMAP_P_PROPS_CORRECT_2_1:
      return QCxml_error_schemap_p_props_correct_2_1;
    case XML_SCHEMAP_P_PROPS_CORRECT_2_2:
      return QCxml_error_schemap_p_props_correct_2_2;
    case XML_SCHEMAP_E_PROPS_CORRECT_2:
      return QCxml_error_schemap_e_props_correct_2;
    case XML_SCHEMAP_E_PROPS_CORRECT_3:
      return QCxml_error_schemap_e_props_correct_3;
    case XML_SCHEMAP_E_PROPS_CORRECT_4:
      return QCxml_error_schemap_e_props_correct_4;
    case XML_SCHEMAP_E_PROPS_CORRECT_5:
      return QCxml_error_schemap_e_props_correct_5;
    case XML_SCHEMAP_E_PROPS_CORRECT_6:
      return QCxml_error_schemap_e_props_correct_6;
    case XML_SCHEMAP_SRC_INCLUDE:
      return QCxml_error_schemap_src_include;
    case XML_SCHEMAP_SRC_ATTRIBUTE_1:
      return QCxml_error_schemap_src_attribute_1;
    case XML_SCHEMAP_SRC_ATTRIBUTE_2:
      return QCxml_error_schemap_src_attribute_2;
    case XML_SCHEMAP_SRC_ATTRIBUTE_3_1:
      return QCxml_error_schemap_src_attribute_3_1;
    case XML_SCHEMAP_SRC_ATTRIBUTE_3_2:
      return QCxml_error_schemap_src_attribute_3_2;
    case XML_SCHEMAP_SRC_ATTRIBUTE_4:
      return QCxml_error_schemap_src_attribute_4;
    case XML_SCHEMAP_NO_XMLNS:
      return QCxml_error_schemap_no_xmlns;
    case XML_SCHEMAP_NO_XSI:
      return QCxml_error_schemap_no_xsi;
    case XML_SCHEMAP_COS_VALID_DEFAULT_1:
      return QCxml_error_schemap_cos_valid_default_1;
    case XML_SCHEMAP_COS_VALID_DEFAULT_2_1:
      return QCxml_error_schemap_cos_valid_default_2_1;
    case XML_SCHEMAP_COS_VALID_DEFAULT_2_2_1:
      return QCxml_error_schemap_cos_valid_default_2_2_1;
    case XML_SCHEMAP_COS_VALID_DEFAULT_2_2_2:
      return QCxml_error_schemap_cos_valid_default_2_2_2;
    case XML_SCHEMAP_CVC_SIMPLE_TYPE:
      return QCxml_error_schemap_cvc_simple_type;
    case XML_SCHEMAP_COS_CT_EXTENDS_1_1:
      return QCxml_error_schemap_cos_ct_extends_1_1;
    case XML_SCHEMAP_SRC_IMPORT_1_1:
      return QCxml_error_schemap_src_import_1_1;
    case XML_SCHEMAP_SRC_IMPORT_1_2:
      return QCxml_error_schemap_src_import_1_2;
    case XML_SCHEMAP_SRC_IMPORT_2:
      return QCxml_error_schemap_src_import_2;
    case XML_SCHEMAP_SRC_IMPORT_2_1:
      return QCxml_error_schemap_src_import_2_1;
    case XML_SCHEMAP_SRC_IMPORT_2_2:
      return QCxml_error_schemap_src_import_2_2;
    case XML_SCHEMAP_INTERNAL:
      return QCxml_error_schemap_internal;
    case XML_SCHEMAP_NOT_DETERMINISTIC:
      return QCxml_error_schemap_not_deterministic;
    case XML_SCHEMAP_SRC_ATTRIBUTE_GROUP_1:
      return QCxml_error_schemap_src_attribute_group_1;
    case XML_SCHEMAP_SRC_ATTRIBUTE_GROUP_2:
      return QCxml_error_schemap_src_attribute_group_2;
    case XML_SCHEMAP_SRC_ATTRIBUTE_GROUP_3:
      return QCxml_error_schemap_src_attribute_group_3;
    case XML_SCHEMAP_MG_PROPS_CORRECT_1:
      return QCxml_error_schemap_mg_props_correct_1;
    case XML_SCHEMAP_MG_PROPS_CORRECT_2:
      return QCxml_error_schemap_mg_props_correct_2;
    case XML_SCHEMAP_SRC_CT_1:
      return QCxml_error_schemap_src_ct_1;
    case XML_SCHEMAP_DERIVATION_OK_RESTRICTION_2_1_3:
      return QCxml_error_schemap_derivation_ok_restriction_2_1_3;
    case XML_SCHEMAP_AU_PROPS_CORRECT_2:
      return QCxml_error_schemap_au_props_correct_2;
    case XML_SCHEMAP_A_PROPS_CORRECT_2:
      return QCxml_error_schemap_a_props_correct_2;
    case XML_SCHEMAP_C_PROPS_CORRECT:
      return QCxml_error_schemap_c_props_correct;
    case XML_SCHEMAP_SRC_REDEFINE:
      return QCxml_error_schemap_src_redefine;
    case XML_SCHEMAP_SRC_IMPORT:
      return QCxml_error_schemap_src_import;
    case XML_SCHEMAP_WARN_SKIP_SCHEMA:
      return QCxml_error_schemap_warn_skip_schema;
    case XML_SCHEMAP_WARN_UNLOCATED_SCHEMA:
      return QCxml_error_schemap_warn_unlocated_schema;
    case XML_SCHEMAP_WARN_ATTR_REDECL_PROH:
      return QCxml_error_schemap_warn_attr_redecl_proh;
    case XML_SCHEMAP_WARN_ATTR_POINTLESS_PROH:
      return QCxml_error_schemap_warn_attr_pointless_proh;
    case XML_SCHEMAP_AG_PROPS_CORRECT:
      return QCxml_error_schemap_ag_props_correct;
    case XML_SCHEMAP_COS_CT_EXTENDS_1_2:
      return QCxml_error_schemap_cos_ct_extends_1_2;
    case XML_SCHEMAP_AU_PROPS_CORRECT:
      return QCxml_error_schemap_au_props_correct;
    case XML_SCHEMAP_A_PROPS_CORRECT_3:
      return QCxml_error_schemap_a_props_correct_3;
    case XML_SCHEMAP_COS_ALL_LIMITED:
      return QCxml_error_schemap_cos_all_limited;
    case XML_SCHEMATRONV_ASSERT:
      return QCxml_error_schematronv_assert;
    case XML_SCHEMATRONV_REPORT:
      return QCxml_error_schematronv_report;
    case XML_MODULE_OPEN:
      return QCxml_error_module_open;
    case XML_MODULE_CLOSE:
      return QCxml_error_module_close;
    case XML_CHECK_FOUND_ELEMENT:
      return QCxml_error_check_found_element;
    case XML_CHECK_FOUND_ATTRIBUTE:
      return QCxml_error_check_found_attribute;
    case XML_CHECK_FOUND_TEXT:
      return QCxml_error_check_found_text;
    case XML_CHECK_FOUND_CDATA:
      return QCxml_error_check_found_cdata;
    case XML_CHECK_FOUND_ENTITYREF:
      return QCxml_error_check_found_entityref;
    case XML_CHECK_FOUND_ENTITY:
      return QCxml_error_check_found_entity;
    case XML_CHECK_FOUND_PI:
      return QCxml_error_check_found_pi;
    case XML_CHECK_FOUND_COMMENT:
      return QCxml_error_check_found_comment;
    case XML_CHECK_FOUND_DOCTYPE:
      return QCxml_error_check_found_doctype;
    case XML_CHECK_FOUND_FRAGMENT:
      return QCxml_error_check_found_fragment;
    case XML_CHECK_FOUND_NOTATION:
      return QCxml_error_check_found_notation;
    case XML_CHECK_UNKNOWN_NODE:
      return QCxml_error_check_unknown_node;
    case XML_CHECK_ENTITY_TYPE:
      return QCxml_error_check_entity_type;
    case XML_CHECK_NO_PARENT:
      return QCxml_error_check_no_parent;
    case XML_CHECK_NO_DOC:
      return QCxml_error_check_no_doc;
    case XML_CHECK_NO_NAME:
      return QCxml_error_check_no_name;
    case XML_CHECK_NO_ELEM:
      return QCxml_error_check_no_elem;
    case XML_CHECK_WRONG_DOC:
      return QCxml_error_check_wrong_doc;
    case XML_CHECK_NO_PREV:
      return QCxml_error_check_no_prev;
    case XML_CHECK_WRONG_PREV:
      return QCxml_error_check_wrong_prev;
    case XML_CHECK_NO_NEXT:
      return QCxml_error_check_no_next;
    case XML_CHECK_WRONG_NEXT:
      return QCxml_error_check_wrong_next;
    case XML_CHECK_NOT_DTD:
      return QCxml_error_check_not_dtd;
    case XML_CHECK_NOT_ATTR:
      return QCxml_error_check_not_attr;
    case XML_CHECK_NOT_ATTR_DECL:
      return QCxml_error_check_not_attr_decl;
    case XML_CHECK_NOT_ELEM_DECL:
      return QCxml_error_check_not_elem_decl;
    case XML_CHECK_NOT_ENTITY_DECL:
      return QCxml_error_check_not_entity_decl;
    case XML_CHECK_NOT_NS_DECL:
      return QCxml_error_check_not_ns_decl;
    case XML_CHECK_NO_HREF:
      return QCxml_error_check_no_href;
    case XML_CHECK_WRONG_PARENT:
      return QCxml_error_check_wrong_parent;
    case XML_CHECK_NS_SCOPE:
      return QCxml_error_check_ns_scope;
    case XML_CHECK_NS_ANCESTOR:
      return QCxml_error_check_ns_ancestor;
    case XML_CHECK_NOT_UTF8:
      return QCxml_error_check_not_utf8;
    case XML_CHECK_NO_DICT:
      return QCxml_error_check_no_dict;
    case XML_CHECK_NOT_NCNAME:
      return QCxml_error_check_not_ncname;
    case XML_CHECK_OUTSIDE_DICT:
      return QCxml_error_check_outside_dict;
    case XML_CHECK_WRONG_NAME:
      return QCxml_error_check_wrong_name;
    case XML_CHECK_NAME_NOT_NULL:
      return QCxml_error_check_name_not_null;
    case XML_I18N_NO_NAME:
      return QCxml_error_i18n_no_name;
    case XML_I18N_NO_HANDLER:
      return QCxml_error_i18n_no_handler;
    case XML_I18N_EXCESS_HANDLER:
      return QCxml_error_i18n_excess_handler;
    case XML_I18N_CONV_FAILED:
      return QCxml_error_i18n_conv_failed;
    case XML_I18N_NO_OUTPUT:
      return QCxml_error_i18n_no_output;
    case XML_BUF_OVERFLOW:
      return QCxml_error_buf_overflow;

    default:
      /* Unknown error. Just return the integer. */
      return make_number (code);
    }
}

#endif /* HAVE_LIBXML2 */
