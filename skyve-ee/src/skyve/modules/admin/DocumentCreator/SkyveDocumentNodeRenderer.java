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
import org.commonmark.node.OrderedList;
import org.commonmark.node.Paragraph;
import org.commonmark.node.Text;
import org.commonmark.renderer.NodeRenderer;
import org.commonmark.renderer.html.HtmlNodeRendererContext;
import org.commonmark.renderer.html.HtmlWriter;
import org.skyve.impl.bind.BindUtil;

public class SkyveDocumentNodeRenderer implements NodeRenderer {

	private final HtmlWriter html;
	private Map<String, String> documentAttributes;
	private Map<String, String> alertText;

	/**
	 * Show indentation for formatting on screen if true. If false, will be hidden and
	 * not printed to file.
	 */
	private boolean htmlSpacing = false;

	private static final String DOCUMENT_DEFINITION = "<document name=\"%s\" "
			+ "xmlns=\"http://www.skyve.org/xml/document\" "
			+ "xsi:schemaLocation=\"http://www.skyve.org/xml/document ../../../schemas/document.xsd\" "
			+ "xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">";
	private static final String PERSISTENT = "<persistent name=\"%s\" />";
	private static final String SINGULAR_ALIAS = "<singularAlias>%s</singularAlias>";
	private static final String PLURAL_ALIAS = "<pluralAlias>%ss</pluralAlias>";
	private static final String ICON = "<iconStyleClass>fa fa-file-o</iconStyleClass>";
	private static final String BIZKEY = "<bizKey expression=\"%s\"/>";

	public SkyveDocumentNodeRenderer(HtmlNodeRendererContext context) {
		this.html = context.getWriter();

		this.documentAttributes = new HashMap<>();
		documentAttributes.put("xmlns", "http://www.skyve.org/xml/document");
		documentAttributes.put("xsi:schemaLocation", "http://www.skyve.org/xml/document ../../../schemas/document.xsd");
		documentAttributes.put("xmlns:xsi", "http://www.w3.org/2001/XMLSchema-instance");

		this.alertText = new HashMap<>();
		alertText.put("style", "color: red");
	}

	public SkyveDocumentNodeRenderer(HtmlNodeRendererContext context, boolean htmlSpacing) {
		this(context);
		this.htmlSpacing = htmlSpacing;
	}

	@Override
	public Set<Class<? extends Node>> getNodeTypes() {
		// return the node types we want to use this renderer for
		Set<Class<? extends Node>> types = new HashSet<>();
		types.add(Heading.class);
		types.add(ListItem.class);
		types.add(BulletList.class);
		types.add(OrderedList.class);

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
					linebreak();

					html.text(String.format(DOCUMENT_DEFINITION, text.getLiteral()));
					linebreak();
					
					// insert persistent name if provided
					if (persistentName != null) {
						tab();
						html.text(String.format(PERSISTENT, persistentName.getLiteral()));
						linebreak();
					}

					tab();
					html.text(String.format(SINGULAR_ALIAS, toTitleCase(text.getLiteral())));
					linebreak();

					tab();
					html.text(String.format(PLURAL_ALIAS, toTitleCase(text.getLiteral())));
					linebreak();

					tab();
					html.text(ICON);
					linebreak();

					tab();
					html.text(String.format(BIZKEY, text.getLiteral()));
					linebreak();
				}
			}
		} else if (node instanceof BulletList) {
			if (node.getPrevious() instanceof Heading) {
				tab();
				html.text("<attributes>");
				linebreak();
			}
			
			// is this a scalar or association, or a collection
			BulletList list = (BulletList) node;
			if (list.getBulletMarker() != '-' && list.getBulletMarker() != '+') {
				html.tag("span", alertText);
				html.text(
						String.format("Unknown list item type: \"%s\". Please use either \"-\" or \"+\".", list.getBulletMarker()));
				html.tag("/span");
				linebreak();
			} else {
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
				if (node.getNext() == null || isNodeHeading2(node.getNext())) {
					writeDocumentEnd();
				}
			}
		} else if (node instanceof OrderedList) {
			// check if the previous node was a bullet list (scalar attribute or association)
			// or a heading (document definition)
			/*if (isNodeHeading2(node.getPrevious()) || isNodeBulletList(node.getPrevious())) {
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
				if (node.getNext() == null || isNodeHeading2(node.getNext())) {
					writeDocumentEnd();
				}
			}*/
			OrderedList list = (OrderedList) node;
			html.tag("span", alertText);
			html.text(
					String.format("Unknown list item type: \"%s%s\". Please use either \"-\" or \"+\".", list.getStartNumber(),
							list.getDelimiter()));
			html.tag("/span");
			linebreak();
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

	private void createAttribute(String[] parts, Node line) {
		// first we have to get the attribute name
		if (parts != null && parts.length > 1) {
			String attributeName = parts[0];
			// remove the first element in the array
			ArrayList<String> pList = new ArrayList<>(Arrays.asList(parts));
			pList.remove(0);
			String[] reducedParts = new String[pList.size()];
			pList.toArray(reducedParts);
			createAttribute(attributeName, reducedParts, false, line);
		}
	}

	private void createAttribute(String attributeName, String[] parts, boolean required, Node line) {
		// identify the type from the parts
		String type = null;
		if (parts.length == 1 || parts.length == 2) {
			type = parts[0];
		}

		if (type != null) {
			// check if this is a scalar attribute
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
					// output attribute name and display name for simple attributes
					writeScalarAttributeStart(type, attributeName, required);
					writeScalarAttributeEnd(type);
					break;
				case "enum":
					// check we have the values specified in brackets
					if(parts.length == 2) {

						String brackets = parts[1];
						if (brackets.startsWith("(") && brackets.endsWith(")")) {
							writeScalarAttributeStart(type, attributeName, required);
							String[] values = brackets.split(",");
							if (values.length > 0) {
								tab(3);
								html.text("<values>");
								linebreak();
								
								for (String v : values) {
									v = v.trim().replace("(", "").replace(")", "");
									tab(4);
									html.text(String.format("<value code=\"%s\"/>", v));
									linebreak();
								}
								
								tab(3);
								html.text("<values>");
								linebreak();
							}
							writeScalarAttributeEnd(type);
						} else {
							html.tag("span", alertText);
							html.text(String.format("Enum attribute: %s missing required value array", attributeName));
							html.tag("/span");
							linebreak();
						}
					}
					break;
				case "text":
					// check we have a length
					if (parts.length == 2) {
						writeScalarAttributeStart(type, attributeName, required);
						tab(3);
						html.text(String.format("<length>%s</length>", parts[1]));
						linebreak();
						writeScalarAttributeEnd(type);
					} else {
						html.tag("span", alertText);
						html.text(String.format("Text attribute: %s missing required field length", attributeName));
						html.tag("/span");
						linebreak();
					}
					break;
				default:
					// did not match scalar attribute, check if association or collection definition
					if (isAssociationDefinition(line, type, parts)) {
						writeAssociation(type, attributeName, required);
					} else if (isCollectionDefinition(line, type, parts)) {
						writeCollection(type, attributeName, required);
					} else {
						html.tag("span", alertText);
						html.text(String.format("Unsupported attribute: %s [%s]", attributeName, parts[0]));
						html.tag("/span");
						linebreak();
					}
					break;
			}
		}
	}

	/**
	 * Returns true if the specified type describes an association:
	 * <ul>
	 * <li>the attribute belongs to an unordered list
	 * <li>the first letter is uppercase
	 * <li>there are no following words on the line
	 * </ul>
	 * 
	 * @param type The type to be checked
	 * @param parts The remaining tokens for the line
	 * @return true if an association, false otherwise
	 */
	private static boolean isAssociationDefinition(Node line, String type, String[] parts) {
		if (isChildOfDashMarkerList(line)) {
			// if (isChildOfBulletList(line)) {
			if (type != null && Character.isUpperCase(type.charAt(0)) && parts.length == 1) {
				return true;
			}
		}

		return false;
	}

	/**
	 * Returns true if the specified node is a BulletList (
	 * <ul>
	 * ).
	 */
	private static boolean isNodeBulletList(Node node) {
		if (node instanceof BulletList) {
			return true;
		}
		return false;
	}

	/**
	 * Recursively checks the parent of the specified node
	 * to see if it is a descendant of a bullet list (
	 * <ul>
	 * ).
	 */
	private static boolean isChildOfBulletList(Node node) {
		if (node.getParent() != null) {
			if (node.getParent() instanceof BulletList) {
				return true;
			}
			return isChildOfBulletList(node.getParent());
		}
		return false;
	}

	/**
	 * Recursively checks the parent of the specified node
	 * to see if it is a descendant of an a bullet list (
	 * <ul>
	 * ) specified with a -.
	 */
	private static boolean isChildOfDashMarkerList(Node node) {
		if (node.getParent() != null) {
			if (node.getParent() instanceof BulletList) {
				BulletList list = (BulletList) node.getParent();
				return list.getBulletMarker() == '-';
			}
			return isChildOfDashMarkerList(node.getParent());
		}
		return false;
	}

	/**
	 * Recursively checks the parent of the specified node
	 * to see if it is a descendant of an ordered list (
	 * <ol>
	 * ).
	 */
	private static boolean isChildOfOrderedList(Node node) {
		if (node.getParent() != null) {
			if (node.getParent() instanceof OrderedList) {
				return true;
			}
			return isChildOfOrderedList(node.getParent());
		}
		return false;
	}

	/**
	 * Recursively checks the parent of the specified node
	 * to see if it is a descendant of an a bullet list (
	 * <ul>
	 * ) specified with a +.
	 */
	private static boolean isChildOfPlusMarkerList(Node node) {
		if (node.getParent() != null) {
			if (node.getParent() instanceof BulletList) {
				BulletList list = (BulletList) node.getParent();
				return list.getBulletMarker() == '+';
			}
			return isChildOfPlusMarkerList(node.getParent());
		}
		return false;
	}

	/**
	 * Returns true if the specified type describes a acollection:
	 * <ul>
	 * <li>the attribute belongs to an ordered list
	 * <li>the first letter is uppercase
	 * <li>there are no following words on the line
	 * </ul>
	 * 
	 * @return true if a collection, false otherwise
	 */
	private static boolean isCollectionDefinition(Node line, String type, String[] parts) {
		if (isChildOfPlusMarkerList(line)) {
			// if (isChildOfOrderedList(line)) {
			if (type != null && Character.isUpperCase(type.charAt(0)) && parts.length == 1) {
				return true;
			}
		}

		return false;
	}

	/**
	 * Returns true if the specified node is a Heading with level 2,
	 * indicating a document definition.
	 */
	private static boolean isNodeHeading2(Node node) {
		if (node instanceof Heading) {
			Heading heading = (Heading) node;
			if (heading.getLevel() == 2) {
				return true;
			}
		}
		return false;
	}

	/**
	 * Outputs a newline to the writer
	 */
	private void linebreak() {
		html.line();
		if (htmlSpacing) {
			html.tag("br");
		}
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
				createAttribute(attributeName, parts, true, em);
			} else {
				html.text("Invalid attribute definition: " + node.getLastChild().toString());
				linebreak();
			}

		} else if (node.getFirstChild() != null && node.getFirstChild() instanceof Text) {
			String line = getTextFromNode(node.getFirstChild());
			String[] parts = line.split("\\s");
			createAttribute(parts, node.getFirstChild());
		}
	}

	/**
	 * Outputs a tab to the writer
	 */
	private void tab(int... tabCount) {
		int numberOfTabs = 1;
		if (tabCount != null && tabCount.length == 1) {
			numberOfTabs = tabCount[0];
		}
		for (int i = 0; i < numberOfTabs; i++) {
			if (htmlSpacing) {
				html.raw("&emsp;");
			} else {
				html.text("\t");
			}
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

	private void writeAssociation(final String type, final String attributeName, final boolean required) {
		// association start
		if (required) {
			tab(2);
			html.text(String.format("<association name=\"%s\" type=\"aggregation\" required=\"true\">", attributeName));
		} else {
			tab(2);
			html.text(String.format("<association name=\"%s\" type=\"aggregation\">", attributeName));
		}
		linebreak();
		tab(3);
		html.text(String.format("<displayName>%s</displayName>", toTitleCase(attributeName)));
		linebreak();

		// write document name
		tab(3);
		html.text(String.format("<documentName>%s</documentName>", type));
		linebreak();

		// association end
		tab(2);
		html.text("</association>");
		linebreak();
	}

	private void writeCollection(final String type, final String attributeName, final boolean required) {
		// collection start
		tab(2);
		html.text(String.format("<collection name=\"%s\" type=\"aggregation\">", attributeName));

		linebreak();
		tab(3);
		html.text(String.format("<displayName>%s</displayName>", toTitleCase(attributeName)));
		linebreak();

		// write document name
		tab(3);
		html.text(String.format("<documentName>%s</documentName>", type));
		linebreak();

		// requiredness
		tab(3);
		html.text("<minCardinality>");
		html.text(required ? "1" : "0");
		html.text("</minCardinality>");
		linebreak();

		// collection end
		tab(2);
		html.text("</collection>");
		linebreak();
	}

	private void writeDocumentEnd() {
		tab();
		html.text("</attributes>");
		linebreak();

		html.text("</document>");
		linebreak();
	}

	private void writeScalarAttributeEnd(final String type) {
		tab(2);
		html.text(String.format("</%s>", type));
		linebreak();
	}

	private void writeScalarAttributeStart(final String type, final String attributeName, final boolean required) {
		if (required) {
			tab(2);
			html.text(String.format("<%s name=\"%s\" required=\"true\">", type, attributeName));
		} else {
			tab(2);
			html.text(String.format("<%s name=\"%s\">", type, attributeName));
		}
		linebreak();
		tab(3);
		html.text(String.format("<displayName>%s</displayName>", toTitleCase(attributeName)));
		linebreak();
	}
}
