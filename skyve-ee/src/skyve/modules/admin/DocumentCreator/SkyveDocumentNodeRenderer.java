package modules.admin.DocumentCreator;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.commonmark.node.BulletList;
import org.commonmark.node.Code;
import org.commonmark.node.Emphasis;
import org.commonmark.node.Heading;
import org.commonmark.node.ListItem;
import org.commonmark.node.Node;
import org.commonmark.node.Paragraph;
import org.commonmark.node.Text;
import org.commonmark.renderer.NodeRenderer;
import org.commonmark.renderer.html.HtmlNodeRendererContext;
import org.commonmark.renderer.html.HtmlWriter;
import org.skyve.impl.bind.BindUtil;

public class SkyveDocumentNodeRenderer implements NodeRenderer {

	private final HtmlWriter html;
	private Map<String, String> documentAttributes;

	private static final String DOCUMENT_DEFINITION = "<document name=\"%s\" "
			+ "xmlns=\"http://www.skyve.org/xml/document\" "
			+ "xsi:schemaLocation=\"http://www.skyve.org/xml/document ../../../schemas/document.xsd\" "
			+ "xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">";
	private static final String PERSISTENT = "\t<persistent name=\"%s\" />";
	private static final String SINGULAR_ALIAS = "\t<singularAlias>%s</singularAlias>";
	private static final String PLURAL_ALIAS = "\t<pluralAlias>%ss</pluralAlias>";
	private static final String ICON = "\t<iconStyleClass>fa fa-file-o</iconStyleClass>";
	private static final String BIZKEY = "\t<bizKey expression=\"%s\"/>";

	public SkyveDocumentNodeRenderer(HtmlNodeRendererContext context) {
		this.html = context.getWriter();

		this.documentAttributes = new HashMap<>();
		documentAttributes.put("xmlns", "http://www.skyve.org/xml/document");
		documentAttributes.put("xsi:schemaLocation", "http://www.skyve.org/xml/document ../../../schemas/document.xsd");
		documentAttributes.put("xmlns:xsi", "http://www.w3.org/2001/XMLSchema-instance");
	}

	@Override
	public Set<Class<? extends Node>> getNodeTypes() {
		// return the node types we want to use this renderer for
		Set<Class<? extends Node>> types = new HashSet<>();
		types.add(Heading.class);
		types.add(ListItem.class);
		types.add(BulletList.class);

		return types;
	}

	@Override
	public void render(Node node) {
		if (node instanceof Heading) {
			Heading heading = (Heading) node;
			if (heading.getLevel() == 2) {
				if (heading.getFirstChild() != null && heading.getFirstChild() instanceof Text) {
					Text text = (Text) heading.getFirstChild();

					Code persistentName = null;
					if (text.getNext() != null && text.getNext() instanceof Code) {
						persistentName = (Code) text.getNext();
					}

					html.line();
					html.text("<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>");
					html.line();
					html.tag("br");

					html.text(String.format(DOCUMENT_DEFINITION, text.getLiteral()));
					html.line();
					html.tag("br");
					
					// insert persistent name if provided
					if (persistentName != null) {
						html.text(String.format(PERSISTENT, persistentName.getLiteral()));
						html.line();
						html.tag("br");
					}

					html.text(String.format(SINGULAR_ALIAS, toTitleCase(text.getLiteral())));
					html.line();
					html.tag("br");

					html.text(String.format(PLURAL_ALIAS, toTitleCase(text.getLiteral())));
					html.line();
					html.tag("br");

					html.text(ICON);
					html.line();
					html.tag("br");

					html.text(String.format(BIZKEY, text.getLiteral()));
					html.line();
					html.tag("br");
				}
			}
		}
		else if (node instanceof BulletList) {
			html.text("<attributes>");
			html.line();
			html.tag("br");

			// get all the list items for this list
			if (node.getFirstChild() != null && node.getFirstChild() instanceof ListItem) {
				ListItem item = (ListItem) node.getFirstChild();
				parseListItem(item);

				while (item.getNext() != null) {
					item = (ListItem) item.getNext();
					parseListItem(item);
				}
			}

			// end the attributes when we run out
			html.text("</attributes>");
			html.line();
			html.tag("br");
			
			html.text("</document>");
			html.line();
			html.tag("br");
		}
	}

	private void parseListItem(ListItem item) {
		if (item.getFirstChild() != null
				&& (item.getFirstChild() instanceof Text || item.getFirstChild() instanceof Paragraph)) {
			if (item.getFirstChild() instanceof Paragraph) {
				Node paragraph = item.getFirstChild();
				parseAttribute(paragraph);
			} else {
				parseAttribute(item);
			}
		}
	}

	/**
	 * Returns the literal text within a Node if the node is not null and of type Text.
	 * 
	 * @param node The node to check for text
	 * @return The text
	 */
	private static String getTextFromNode(Node node) {
		if (node != null && node instanceof Text) {
			Text text = (Text) node;
			return text.getLiteral();
		}

		return null;
	}

	/**
	 * Creates an <attribute> from the parent Node
	 * 
	 * @param node The node which is assumed to be the start of the attribute definition
	 */
	private void parseAttribute(Node node) {
		if (node.getFirstChild() != null && node.getFirstChild() instanceof Emphasis) {
			// required attribute
			Emphasis em = (Emphasis) node.getFirstChild();
			String attributeName = getTextFromNode(em.getFirstChild());

			// get the rest of the attribute spec
			if (em.getNext() != null && em.getNext() instanceof Text) {
				String remainingDefinition = getTextFromNode(em.getNext());
				String[] parts = remainingDefinition.trim().split("\\s");
				createAttribute(attributeName, parts, true);
			} else {
				html.text("Invalid attribute definition: " + node.getLastChild().toString());
				html.line();
				html.tag("br");
			}

		} else if (node.getFirstChild() != null && node.getFirstChild() instanceof Text) {
			String line = getTextFromNode(node.getFirstChild());
			String[] parts = line.split("\\s");
			createAttribute(parts);
		}
	}

	private void createAttribute(String[] parts) {
		// first we have to get the attribute name
		if (parts != null && parts.length > 1) {
			String attributeName = parts[0];
			// remove the first element in the array
			ArrayList<String> pList = new ArrayList<>(Arrays.asList(parts));
			pList.remove(0);
			String[] reducedParts = new String[pList.size()];
			pList.toArray(reducedParts);
			createAttribute(attributeName, reducedParts, false);
		}
	}

	private void createAttribute(String attributeName, String[] parts, boolean required) {
		// identify the type from the parts
		String type = null;
		if (parts.length == 1 || parts.length == 2) {
			type = parts[0];
		}

		if (type != null) {
			writeScalarAttributeStart(type, attributeName, required);
			switch (type) {
				case "boolean":
				case "date":
				case "dateTime":
				case "decimal10":
				case "decimal2":
				case "decimal5":
				case "geometry":
				case "id":
				case "integer":
				case "longInteger":
				case "memo":
				case "markup":
				case "time":
				case "timestamp":
					break;
				case "enum":
					// check we have the values specified in brackets
					if(parts.length == 2) {
						String brackets = parts[1];
						if (brackets.startsWith("(") && brackets.endsWith(")")) {
							String[] values = brackets.split(",");
							if (values.length > 0) {
								html.text("\t\t\t<values>");
								html.line();
								html.tag("br");
								
								for (String v : values) {
									v = v.trim().replace("(", "").replace(")", "");
									html.text(String.format("\t\t\t<value code=\"%s\"/>", v));
									html.line();
									html.tag("br");
								}
								
								html.text("\t\t\t<values>");
								html.line();
								html.tag("br");
							}
						}
					}
					break;
				case "text":
					// check we have a length
					if (parts.length == 2) {
						html.text(String.format("\t\t\t<length>%s</length>", parts[1]));
						html.line();
						html.tag("br");
					}

					break;
				default:
					Map<String, String> alertText = new HashMap<>();
					alertText.put("style", "color: red");
					html.tag("span", alertText);
					html.text(String.format("Unsupported attribute: %s [%s]", attributeName, parts[0]));
					html.tag("/span");
					html.line();
					html.tag("br");
					break;
			}
			writeScalarAttributeEnd(type);
		}
	}

	/**
	 * Fashion a title case identifier from the given string.
	 * 
	 * @param string The string to convert
	 * @return A valid java static identifier. First letter upper case words with spaces between.
	 */
	private static String toTitleCase(String string) {
		String javaIdentifierName = BindUtil.toJavaTypeIdentifier(string);
		StringBuilder sb = new StringBuilder(javaIdentifierName.length() + 5);

		for (int i = 0, l = javaIdentifierName.length(); i < l; i++) {
			char ch = javaIdentifierName.charAt(i);
			if (Character.isUpperCase(ch)) {
				boolean nextCharLowerCase = false;
				boolean prevCharLowerCase = false;
				int nextIndex = i + 1;
				int prevIndex = i - 1;
				if (nextIndex < l) {
					char nextChar = javaIdentifierName.charAt(nextIndex);
					nextCharLowerCase = Character.isLowerCase(nextChar);
				}
				if (prevIndex >= 0) {
					char prevChar = javaIdentifierName.charAt(prevIndex);
					prevCharLowerCase = Character.isLowerCase(prevChar);
				}

				// if the previous char was upper case then don't add a space
				if ((prevCharLowerCase || nextCharLowerCase) && (i > 0)) {
					sb.append(' ');
				}
				sb.append(ch);
			} else {
				sb.append(ch);
			}
		}

		return sb.toString();
	}

	private void writeScalarAttributeEnd(final String type) {
		html.text(String.format("\t\t</%s>", type));
		html.line();
		html.tag("br");
	}

	private void writeScalarAttributeStart(final String type, final String attributeName, final boolean required) {
		if (required) {
			html.text(String.format("\t\t<%s name=\"%s\" required=\"true\">", type, attributeName));
		} else {
			html.text(String.format("\t\t<%s name=\"%s\">", type, attributeName));
		}
		html.line();
		html.tag("br");
		html.text(String.format("\t\t\t<displayName>%s</displayName>", toTitleCase(attributeName)));
		html.line();
		html.tag("br");
	}
}
