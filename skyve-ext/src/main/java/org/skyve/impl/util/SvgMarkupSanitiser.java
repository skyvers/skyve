package org.skyve.impl.util;

import java.io.IOException;
import java.io.StringReader;
import java.io.StringWriter;
import java.nio.charset.StandardCharsets;
import java.util.Base64;
import java.util.Locale;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.xml.XMLConstants;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Restricts SVG image markup to static drawing content before it is stored against an image.
 * <p>
 * Image markup is standard SVG drawn over an image (see {@code AttachmentContent.getMarkup()}).
 * It can be written by the browser's image editors, so it is untrusted input.
 * This keeps the SVG elements and presentation attributes used for drawing and removes everything else:
 * scripts, event handlers, styles sheets, foreign content, external references and links.
 * Comments are kept, so editor state embedded in a comment (such as Excalidraw's scene) survives.
 * <p>
 * The class is stateless and thread safe.
 */
public final class SvgMarkupSanitiser {
	/**
	 * The longest markup accepted, in characters.
	 */
	public static final int MAXIMUM_LENGTH = 8 * 1024 * 1024;

	private static final String SVG_NAMESPACE = "http://www.w3.org/2000/svg";
	private static final String XLINK_NAMESPACE = "http://www.w3.org/1999/xlink";
	private static final String HREF = "href";
	private static final String SVG_DATA_URL_PREFIX = "data:image/svg+xml;base64,";
	// SVG images embedded in markup can themselves embed SVG images; stop after this many levels.
	private static final int MAXIMUM_NESTING = 3;

	private static final Set<String> ELEMENTS = Set.of("svg", "g", "defs", "metadata", "title", "desc",
														"path", "line", "polyline", "polygon", "rect", "circle", "ellipse",
														"text", "tspan", "symbol", "use", "image",
														"mask", "clipPath", "linearGradient", "radialGradient", "stop",
														"marker", "pattern");

	// Elements that are replaced by their children
	private static final Set<String> UNWRAPPED_ELEMENTS = Set.of("a", "switch");

	private static final Set<String> ATTRIBUTES = Set.of("id", "class", "version", "viewBox", "preserveAspectRatio",
															"x", "y", "x1", "y1", "x2", "y2", "cx", "cy", "r", "rx", "ry",
															"width", "height", "d", "points", "transform", "pathLength",
															"fill", "fill-opacity", "fill-rule",
															"stroke", "stroke-width", "stroke-opacity", "stroke-linecap",
															"stroke-linejoin", "stroke-dasharray", "stroke-dashoffset",
															"stroke-miterlimit", "opacity", "color", "visibility", "display",
															"vector-effect", "paint-order", "style", "mask", "clip-path",
															"clip-rule", "filter", "font-family", "font-size", "font-weight",
															"font-style", "text-anchor", "dominant-baseline", "alignment-baseline",
															"direction", "dir", "letter-spacing", "word-spacing", "text-decoration",
															"dx", "dy", "rotate", "textLength", "lengthAdjust",
															"offset", "stop-color", "stop-opacity",
															"gradientUnits", "gradientTransform", "spreadMethod", "fx", "fy", "fr",
															"maskUnits", "maskContentUnits", "clipPathUnits",
															"patternUnits", "patternContentUnits", "patternTransform",
															"markerWidth", "markerHeight", "markerUnits", "refX", "refY", "orient",
															"marker-start", "marker-mid", "marker-end");

	private static final Pattern RASTER_DATA_URL = Pattern.compile("^data:image/(?:png|jpeg|gif|webp);base64,[A-Za-z0-9+/=\\s]*$");
	private static final Pattern CSS_URL = Pattern.compile("url\\(\\s*['\"]?\\s*([^)'\"\\s]*)", Pattern.CASE_INSENSITIVE);

	private SvgMarkupSanitiser() {
		// static utility
	}

	/**
	 * Returns the markup with everything except static SVG drawing content removed.
	 * <p>
	 * The root element must be an SVG {@code svg} element. Document type declarations are refused,
	 * so external entities are never resolved.
	 *
	 * @param svg	the markup to sanitise, or {@code null}
	 * @return	the sanitised markup, or {@code null} when {@code svg} is {@code null} or blank
	 * @throws IllegalArgumentException	if the markup is longer than {@link #MAXIMUM_LENGTH},
	 * 										is not well formed XML, has a document type declaration
	 * 										or is not an SVG document
	 */
	public static @Nullable String sanitise(@Nullable String svg) {
		String value = UtilImpl.processStringValue(svg);
		if (value == null) {
			return null;
		}
		return sanitise(value, 0);
	}

	private static @Nonnull String sanitise(@Nonnull String svg, int nesting) {
		if (svg.length() > MAXIMUM_LENGTH) {
			throw new IllegalArgumentException("SVG markup is longer than " + MAXIMUM_LENGTH + " characters");
		}

		Document document = parse(svg);
		Element root = document.getDocumentElement();
		if ((! SVG_NAMESPACE.equals(root.getNamespaceURI())) || (! "svg".equals(root.getLocalName()))) {
			throw new IllegalArgumentException("Markup is not an SVG document");
		}

		// Remove any processing instructions or comments around the root element
		Node node = document.getFirstChild();
		while (node != null) {
			Node next = node.getNextSibling();
			if (node != root) {
				document.removeChild(node);
			}
			node = next;
		}

		sanitiseElement(root, nesting);
		return serialise(document);
	}

	private static @Nonnull Document parse(@Nonnull String svg) {
		try {
			DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
			factory.setNamespaceAware(true);
			factory.setFeature(XMLConstants.FEATURE_SECURE_PROCESSING, true);
			factory.setFeature("http://apache.org/xml/features/disallow-doctype-decl", true);
			factory.setFeature("http://xml.org/sax/features/external-general-entities", false);
			factory.setFeature("http://xml.org/sax/features/external-parameter-entities", false);
			factory.setXIncludeAware(false);
			factory.setExpandEntityReferences(false);
			DocumentBuilder builder = factory.newDocumentBuilder();
			builder.setErrorHandler(new DefaultHandler()); // throw on fatal errors without printing to stderr
			return builder.parse(new InputSource(new StringReader(svg)));
		}
		catch (ParserConfigurationException | SAXException | IOException e) {
			throw new IllegalArgumentException("SVG markup could not be parsed", e);
		}
	}

	private static @Nonnull String serialise(@Nonnull Document document) {
		try {
			TransformerFactory factory = TransformerFactory.newInstance();
			factory.setFeature(XMLConstants.FEATURE_SECURE_PROCESSING, true);
			factory.setAttribute(XMLConstants.ACCESS_EXTERNAL_DTD, "");
			factory.setAttribute(XMLConstants.ACCESS_EXTERNAL_STYLESHEET, "");
			Transformer transformer = factory.newTransformer();
			transformer.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "yes");
			transformer.setOutputProperty(OutputKeys.INDENT, "no");
			StringWriter result = new StringWriter(1024);
			transformer.transform(new DOMSource(document), new StreamResult(result));
			return result.toString();
		}
		catch (TransformerException e) {
			throw new IllegalArgumentException("SVG markup could not be written", e);
		}
	}

	private static void sanitiseElement(@Nonnull Element element, int nesting) {
		sanitiseAttributes(element, nesting);

		Node child = element.getFirstChild();
		while (child != null) {
			Node next = child.getNextSibling();
			short type = child.getNodeType();
			if (type == Node.ELEMENT_NODE) {
				next = sanitiseChildElement(element, (Element) child, next, nesting);
			}
			else if ((type != Node.TEXT_NODE) && (type != Node.CDATA_SECTION_NODE) && (type != Node.COMMENT_NODE)) {
				element.removeChild(child);
			}
			child = next;
		}
	}

	/**
	 * Keeps and sanitises a drawing element, replaces an unwrapped element by its children, or removes anything else.
	 * 
	 * @return	the node to carry on from: the next sibling, or the first of an unwrapped element's children
	 */
	private static @Nullable Node sanitiseChildElement(@Nonnull Element parent, @Nonnull Element child, @Nullable Node next, int nesting) {
		String name = SVG_NAMESPACE.equals(child.getNamespaceURI()) ? child.getLocalName() : null;
		if ((name != null) && ELEMENTS.contains(name)) {
			sanitiseElement(child, nesting);
			return next;
		}
		if ((name != null) && UNWRAPPED_ELEMENTS.contains(name)) {
			// Move the children up in place of the element, then carry on from the first of them
			Node first = child.getFirstChild();
			while (child.getFirstChild() != null) {
				parent.insertBefore(child.getFirstChild(), child);
			}
			parent.removeChild(child);
			return (first != null) ? first : next;
		}
		parent.removeChild(child);
		return next;
	}

	private static void sanitiseAttributes(@Nonnull Element element, int nesting) {
		NamedNodeMap attributes = element.getAttributes();
		for (int i = attributes.getLength() - 1; i >= 0; i--) {
			Attr attribute = (Attr) attributes.item(i);
			if (! keepAttribute(element, attribute, nesting)) {
				element.removeAttributeNode(attribute);
			}
		}
	}

	/**
	 * Whether to keep an attribute: namespace declarations for SVG and XLink, xml:space,
	 * safe references (which may be rewritten in place), data attributes and safe drawing attributes.
	 */
	private static boolean keepAttribute(@Nonnull Element element, @Nonnull Attr attribute, int nesting) {
		String namespace = attribute.getNamespaceURI();
		String name = attribute.getLocalName();
		String value = attribute.getValue();
		if (XMLConstants.XMLNS_ATTRIBUTE_NS_URI.equals(namespace)) {
			return SVG_NAMESPACE.equals(value) || XLINK_NAMESPACE.equals(value);
		}
		if (XMLConstants.XML_NS_URI.equals(namespace)) {
			return "space".equals(name);
		}
		if (HREF.equals(name) && ((namespace == null) || XLINK_NAMESPACE.equals(namespace))) {
			String reference = sanitiseReference(element, value, nesting);
			if (reference == null) {
				return false;
			}
			if (! reference.equals(value)) {
				attribute.setValue(reference);
			}
			return true;
		}
		if ((namespace != null) || (name == null)) {
			return false;
		}
		return name.startsWith("data-") || (ATTRIBUTES.contains(name) && safeValue(value));
	}

	/**
	 * Allows references within the document, and embedded images on image elements.
	 * Embedded SVG images are sanitised too.
	 */
	private static @Nullable String sanitiseReference(@Nonnull Element element, @Nonnull String reference, int nesting) {
		String value = reference.trim();
		if (value.startsWith("#")) {
			return value;
		}
		if (! "image".equals(element.getLocalName())) {
			return null;
		}
		if (RASTER_DATA_URL.matcher(value).matches()) {
			return value;
		}
		if (value.regionMatches(true, 0, SVG_DATA_URL_PREFIX, 0, SVG_DATA_URL_PREFIX.length()) && (nesting < MAXIMUM_NESTING)) {
			try {
				String svg = new String(Base64.getMimeDecoder().decode(value.substring(SVG_DATA_URL_PREFIX.length())), StandardCharsets.UTF_8);
				String sanitised = sanitise(svg, nesting + 1);
				return SVG_DATA_URL_PREFIX + Base64.getEncoder().encodeToString(sanitised.getBytes(StandardCharsets.UTF_8));
			}
			catch (@SuppressWarnings("unused") IllegalArgumentException e) {
				return null;
			}
		}
		return null;
	}

	/**
	 * Refuses script and style tricks, and CSS url() references to anything but an element in the document.
	 */
	private static boolean safeValue(@Nonnull String value) {
		String lowerCaseValue = value.toLowerCase(Locale.ROOT);
		if (lowerCaseValue.contains("javascript:") ||
				lowerCaseValue.contains("vbscript:") ||
				lowerCaseValue.contains("data:") ||
				lowerCaseValue.contains("expression") ||
				lowerCaseValue.contains("@import") ||
				lowerCaseValue.contains("behavior") ||
				lowerCaseValue.contains("-moz-binding") ||
				lowerCaseValue.contains("\\") ||
				lowerCaseValue.contains("<")) {
			return false;
		}
		Matcher matcher = CSS_URL.matcher(value);
		while (matcher.find()) {
			if (! matcher.group(1).startsWith("#")) {
				return false;
			}
		}
		return true;
	}
}
