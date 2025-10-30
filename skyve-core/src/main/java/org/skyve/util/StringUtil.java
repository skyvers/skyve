package org.skyve.util;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.ChildBean;
import org.skyve.domain.types.DateOnly;
import org.skyve.domain.types.Decimal2;
import org.skyve.domain.types.Decimal5;
import org.skyve.domain.types.converters.date.DD_MMM_YYYY;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;

/**
 * General string helper utilities.
 */
public final class StringUtil {

	private StringUtil() {
		/* no instances */ }

	/**
	 * Concatenate non-empty strings with the supplied delimiter. Nulls and empty strings are skipped.
	 * A null delimiter defaults to a single space.
	 */
	public static String concatWithDelim(String delimiter, String... strings) {
		StringBuilder sb = new StringBuilder();
		String delim = Coalesce.coalesceNull(delimiter, " ");
		if (strings != null) {
			for (String s : strings) {
				if (Coalesce.coalesceNull(s, "").length() > 0) {
					if (sb.toString().length() > 0) {
						sb.append(delim);
					}
					sb.append(s);
				}
			}
		}
		return sb.toString();
	}

	/**
	 * Enquote the supplied string using a quote set. If {@code quoteSet} contains 2 characters the first is used as the
	 * left quote and the second (and remaining) as the right quote. If it contains 1 character it is used for both sides.
	 * Null quote set returns the original string.
	 */
	public static String enquote(String quoteSet, String s) {
		String l = null;
		String r = null;
		if (quoteSet != null) {
			l = quoteSet.substring(0, 1);
			if (Coalesce.coalesceNull(quoteSet, "").length() > 1) {
				r = quoteSet.substring(1);
			}
		}
		return concatWithDelim("", l, concatWithDelim("", s, r));
	}

	/**
	 * Title-case the supplied string (first character upper-cased, remainder unchanged). Prefer {@link Binder#toTitleCase(String)}
	 * for full lexical title-casing rules; this method preserves legacy behaviour.
	 */
	public static String titleCase(String raw) {
		String s = raw;
		if (s != null) {
			if (s.length() > 1) {
				s = s.substring(0, 1).toUpperCase() + s.substring(1);
			} else if (s.length() == 1) {
				s = s.toUpperCase();
			}
		}
		return s;
	}

	/**
	 * Replace binding tokens ("{Display Name}" forms) in the provided string by resolving them against the supplied bean.
	 * This mirrors the legacy implementation from {@code ModulesUtil}. New code should consider using richer binding APIs
	 * if available.
	 *
	 * @param bean The root bean for binding resolution.
	 * @param replacementString The string possibly containing replacement tokens.
	 * @return The resolved string.
	 * @throws Exception Metadata/binding resolution failures.
	 */
	public static String replaceBindingsInString(Bean bean, String replacementString) throws Exception {
		StringBuilder result = new StringBuilder(replacementString);
		int openCurlyBraceIndex = result.indexOf("{");

		while (openCurlyBraceIndex >= 0) {
			int closedCurlyBraceIndex = result.indexOf("}");
			String displayNameOfAttribute = result.substring(openCurlyBraceIndex + 1, closedCurlyBraceIndex);

			Bean b = bean;
			StringBuilder binding = new StringBuilder();
			String[] attributes = displayNameOfAttribute.toString().split("\\.");
			boolean found = false;
			for (String a : attributes) {
				if (binding.toString().length() > 0) {
					b = (Bean) Binder.get(bean, binding.toString());
				}
				if ("parent".equals(a)) {
					b = ((ChildBean<?>) bean).getParent();
				}
				found = false;
				if (b != null) {
					User user = CORE.getPersistence().getUser();
					Customer customer = user.getCustomer();
					Module module = customer.getModule(b.getBizModule());
					Document document = module.getDocument(customer, b.getBizDocument());
					for (Attribute attribute : document.getAllAttributes(customer)) {
						if (attribute.getLocalisedDisplayName().equals(a)) {
							found = true;
							if (binding.toString().length() > 0) {
								binding.append('.').append(attribute.getName());
							} else {
								binding.append(attribute.getName());
							}
						}
					}
					if (!found) {
						try {
							if (Binder.get(bean, a) != null) {
								binding.append(a);
							}
						} catch (@SuppressWarnings("unused") Exception e) {
							// ignore optional attribute failures
						}
					}
				}
			}

			String term = "";
			if (found) {
				Object value = Binder.get(bean, binding.toString());
				if (value instanceof DateOnly) {
					DateOnly dValue = (DateOnly) value;
					DD_MMM_YYYY convDate = new DD_MMM_YYYY();
					term = convDate.toDisplayValue(dValue);
				} else if (value instanceof Decimal2) {
					term = value.toString();
				} else if (value instanceof Decimal5) {
					term = value.toString();
				} else {
					term = Coalesce.coalesceNull(value, "").toString();
				}
			}

			String displayValue = Coalesce.coalesceNull(term, "");
			result.replace(openCurlyBraceIndex, closedCurlyBraceIndex + 1, displayValue);
			openCurlyBraceIndex = result.indexOf("{");
		}

		return result.toString();
	}
}
