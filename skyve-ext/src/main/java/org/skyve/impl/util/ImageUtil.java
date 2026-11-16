package org.skyve.impl.util;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Font;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.RenderingHints;
import java.awt.font.FontRenderContext;
import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.StringReader;
import java.io.StringWriter;
import java.io.Writer;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.imageio.ImageIO;
import javax.imageio.ImageReadParam;
import javax.imageio.ImageReader;
import javax.imageio.stream.ImageInputStream;
import javax.xml.XMLConstants;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.apache.batik.transcoder.SVGAbstractTranscoder;
import org.apache.batik.transcoder.TranscoderException;
import org.apache.batik.transcoder.TranscoderInput;
import org.apache.batik.transcoder.TranscoderOutput;
import org.apache.batik.transcoder.image.ImageTranscoder;
import org.apache.commons.io.FilenameUtils;
import org.skyve.CORE;
import org.skyve.metadata.model.document.DynamicImage.ImageFormat;
import org.skyve.metadata.repository.Repository;
import org.skyve.util.FileUtil;
import org.skyve.util.Util;
import org.skyve.util.logging.SkyveLoggerFactory;
import org.slf4j.Logger;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import net.coobird.thumbnailator.util.exif.ExifFilterUtils;
import net.coobird.thumbnailator.util.exif.ExifUtils;
import net.coobird.thumbnailator.util.exif.Orientation;

/**
 * Image Utility methods.
 */
public class ImageUtil {
	private static final Logger LOGGER = SkyveLoggerFactory.getLogger(ImageUtil.class);

	private static final int FIRST_IMAGE_INDEX = 0;
	
	private static final Color TRANSPARENT = new Color(0, 0, 0, 0);

	private static final long MEGABYTE = 1024L * 1024L;

	/**
	 * The smallest thumbnail size - see {@link #thumbnailStep(int)}.
	 */
	private static final int SMALLEST_THUMBNAIL_STEP = 64;

	/**
	 * The most pixels an image can have and still be decoded - 256 megapixels, more than any camera makes.
	 * A bigger image, such as a decompression bomb, is treated as a file that isn't an image.
	 */
	private static final long MAXIMUM_SOURCE_PIXELS = 256L * MEGABYTE;

	private ImageUtil() {}

	/**
	 * Returns the thumbnail size step for a box side: 64, doubling until it covers the side,
	 * where the last step is {@link UtilImpl#THUMBNAIL_MAXIMUM_DIMENSION}.
	 * With the default maximum of 2048 the steps are 64, 128, 256, 512, 1024 and 2048.
	 *
	 * @param side	The box side in pixels.
	 * @return	The smallest step that covers the side, or the maximum if none does.
	 */
	public static int thumbnailStep(int side) {
		int maximum = UtilImpl.THUMBNAIL_MAXIMUM_DIMENSION;
		long step = SMALLEST_THUMBNAIL_STEP; // long so doubling cannot overflow
		while ((step < side) && (step < maximum)) {
			step *= 2;
		}
		return (int) Math.min(step, maximum);
	}

	/**
	 * Returns the square thumbnail size to make for a box asked for in a request: the box's larger side
	 * rounded up to a {@link #thumbnailStep(int) step}. So a request can only choose from a few sizes, and
	 * the thumbnail, which fits inside the square, is never smaller than the one asked for.
	 * A box with only one side given is sized by that side.
	 *
	 * @param width	The requested width in pixels; 0 or less means not given.
	 * @param height	The requested height in pixels; 0 or less means not given.
	 * @return	The side of the square to fit the thumbnail in, or 0 when neither side is given.
	 */
	public static int thumbnailSize(int width, int height) {
		int side = Math.max(width, height);
		return (side <= 0) ? 0 : thumbnailStep(side);
	}

	/**
	 * The most pixels any image is decoded at - (2 x the maximum thumbnail dimension) squared.
	 */
	private static long decodedPixelBudget() {
		long maximum = UtilImpl.THUMBNAIL_MAXIMUM_DIMENSION;
		return 4L * maximum * maximum;
	}

	/**
	 * The size to decode an image side at, for a box side it will be shrunk to fit.
	 * Twice the box side, so the shrink has spare pixels and looks smooth, but no more than the maximum
	 * thumbnail dimension unless the box itself is bigger.
	 */
	private static long decodeTarget(int fitSide) {
		return Math.max(fitSide, Math.min(2L * fitSide, UtilImpl.THUMBNAIL_MAXIMUM_DIMENSION));
	}

	/**
	 * Just like javax.imageio.ImageIO.read() but will subs-sample pixels for large images
	 * to constrain memory usage and apply EXIF rotation.
	 * Whatever its shape, an image is never decoded at more than (2 x {@link UtilImpl#THUMBNAIL_MAXIMUM_DIMENSION})
	 * squared pixels, and an image of more than 256 megapixels is not read at all.
	 * 
	 * @param is	input stream
	 * @param subsamplingMinimumTargetSize	If the images smallest dimension is twice or more this value
	 * 											the image will be sub-sampled as its loaded to conserve memory.
	 * @return	The buffered image, or null if the stream is not a readable image or is too big to read.
	 * @throws IOException
	 */
	public static final @Nullable BufferedImage read(@Nonnull InputStream is, int subsamplingMinimumTargetSize)
	throws IOException {
		return read(is, subsamplingMinimumTargetSize, 0, 0);
	}

	/**
	 * Like {@link #read(InputStream, int)}, but sub-sampled to suit a thumbnail that fits inside the given box.
	 * The image is decoded at between once and twice the size it will be in the box, so that shrinking it
	 * looks smooth, but no bigger than {@link UtilImpl#THUMBNAIL_MAXIMUM_DIMENSION} unless the box needs it.
	 * It is never decoded smaller than it will be in the box, so a thumbnail made from it is never enlarged.
	 * So with the default maximum of 2048, a thumbnail of up to 1024 decodes at most 2048 x 2048 pixels (16MB).
	 * The box applies to the image after its EXIF rotation, and subsamplingMinimumTargetSize is not used.
	 *
	 * @param is	input stream
	 * @param subsamplingMinimumTargetSize	as for {@link #read(InputStream, int)}, used only when there is no box
	 * @param fitWidth	the width of the box the image will be shrunk to fit, or 0 when there is no box
	 * @param fitHeight	the height of the box the image will be shrunk to fit, or 0 when there is no box
	 * @return	The buffered image, or null if the stream is not a readable image or is too big to read.
	 * @throws IOException
	 */
	public static final @Nullable BufferedImage read(@Nonnull InputStream is,
														int subsamplingMinimumTargetSize,
														int fitWidth,
														int fitHeight)
	throws IOException {
		BufferedImage result = null;

		try (ImageInputStream iis = ImageIO.createImageInputStream(is)) {
			Iterator<ImageReader> i = ImageIO.getImageReaders(iis);
			if (i.hasNext()) {
				ImageReader ir = i.next();
				try {
					ir.setInput(iis);
					ImageReadParam irp = ir.getDefaultReadParam();
					int width = ir.getWidth(FIRST_IMAGE_INDEX);
					int height = ir.getHeight(FIRST_IMAGE_INDEX);
					long sourcePixels = (long) width * height;
					if (sourcePixels > MAXIMUM_SOURCE_PIXELS) {
						LOGGER.warn("Not reading an image of {}x{} pixels - the most is {} pixels", Integer.valueOf(width), Integer.valueOf(height), Long.valueOf(MAXIMUM_SOURCE_PIXELS));
						return null;
					}

					Orientation orientation = null;
					try {
						orientation = ExifUtils.getExifOrientation(ir, FIRST_IMAGE_INDEX);
					}
					catch (@SuppressWarnings("unused") Exception e) {
						// do nothing - could not get orientation information so just continue on
					}

					int subsampling = subsampling(width, height, orientation, subsamplingMinimumTargetSize, fitWidth, fitHeight);

					if (subsampling > 1) {
						irp.setSourceSubsampling(subsampling, subsampling, 0, 0);
					}
					
					result = ir.read(FIRST_IMAGE_INDEX, irp);
	
					// Set EXIF
					if ((orientation != null) && (! Orientation.TOP_LEFT.equals(orientation))) {
						result = ExifFilterUtils.getFilterForOrientation(orientation).apply(result);
					}
				}
				finally {
					ir.dispose();
				}
			}
		}

		return result;
	}
	
	/**
	 * The sub-sampling to decode an image at - see {@link #read(InputStream, int, int, int)}.
	 * The result is at least big enough to keep the decoded image within the pixel budget.
	 */
	private static int subsampling(int width,
									int height,
									@Nullable Orientation orientation,
									int subsamplingMinimumTargetSize,
									int fitWidth,
									int fitHeight) {
		int result;
		if ((fitWidth > 0) && (fitHeight > 0)) {
			// A quarter turn swaps the sides that the box applies to
			boolean quarterTurn = Orientation.LEFT_TOP.equals(orientation) ||
									Orientation.RIGHT_TOP.equals(orientation) ||
									Orientation.RIGHT_BOTTOM.equals(orientation) ||
									Orientation.LEFT_BOTTOM.equals(orientation);
			int uprightWidth = quarterTurn ? height : width;
			int uprightHeight = quarterTurn ? width : height;
			// Fitting shrinks by the larger side ratio
			double shrink = Math.max((double) uprightWidth / fitWidth, (double) uprightHeight / fitHeight);
			double toTarget = Math.max((double) uprightWidth / decodeTarget(fitWidth),
										(double) uprightHeight / decodeTarget(fitHeight));
			// Round up so the decoded image is no bigger than the decode target,
			// but never sub-sample by more than the shrink, so it is never smaller than the box needs
			result = (int) Math.max(1, Math.min(Math.ceil(toTarget), Math.floor(shrink)));
		}
		else {
			result = Math.min(width, height) / subsamplingMinimumTargetSize;
		}
		// Never decode more pixels than the budget, whatever the image's shape - eg a long panorama
		int budgetSubsampling = (int) Math.ceil(Math.sqrt((double) width * height / decodedPixelBudget()));
		return Math.max(result, budgetSubsampling);
	}
	
	/**
	 * Return UTF-8 byte array for an SVG representing the type of the file.
	 * The SVG is sized to the largest it can be inside the requested width and height while keeping
	 * the shape of its viewBox, so a browser can scale it to fill any box without padding.
	 * An icon without a viewBox is sized to the requested width and height.
	 *
	 * @param fileName	The filename suffix is used to get the svg file type icon
	 * @param imageWidth	The requested width.
	 * @param imageHeight	The requested height.
	 * @return	A byte[] to stream to a client.
	 * @throws IOException
	 */
	// See https://github.com/dmhendricks/file-icon-vectors/ for icons in /skyve-war/src/main/java/resources/files/
	private static final String BLANK_SVG = "files/blank.svg";

	/**
	 * Captures the value of a viewBox attribute - "min-x min-y width height".
	 */
	private static final Pattern VIEW_BOX = Pattern.compile("viewBox\\s*=\\s*\"([^\"]*)\"");

	public static @Nonnull byte[] svg(@Nonnull String fileName, int imageWidth, int imageHeight)
	throws IOException {
		String suffix = Util.processStringValue(FilenameUtils.getExtension(fileName));
		Repository repository = CORE.getRepository();
		File svgFile = null;
		if (suffix != null) {
			svgFile = repository.findResourceFile("files/" + suffix.toLowerCase() + ".svg", null, null);
		}
		if ((svgFile == null) || (! svgFile.exists())) {
			svgFile = repository.findResourceFile(BLANK_SVG, null, null);
		}
		if (svgFile == null) {
			throw new FileNotFoundException(BLANK_SVG);
		}
		// Add a width and height to the SVG so that it can be scaled
		// and keep its aspect ratio in the browser <img/>.
		String xml = FileUtil.string(svgFile);
		int width = imageWidth;
		int height = imageHeight;
		Matcher viewBox = VIEW_BOX.matcher(xml.substring(0, Math.max(0, xml.indexOf('>'))));
		if ((width > 0) && (height > 0) && viewBox.find()) {
			String[] values = viewBox.group(1).trim().split("[\\s,]+");
			if (values.length == 4) {
				try {
					double viewBoxWidth = Double.parseDouble(values[2]);
					double viewBoxHeight = Double.parseDouble(values[3]);
					if ((viewBoxWidth > 0) && (viewBoxHeight > 0)) {
						double scale = Math.min(width / viewBoxWidth, height / viewBoxHeight);
						width = (int) Math.max(1, Math.round(viewBoxWidth * scale));
						height = (int) Math.max(1, Math.round(viewBoxHeight * scale));
					}
				}
				catch (@SuppressWarnings("unused") NumberFormatException e) {
					// a malformed viewBox keeps the requested width and height
				}
			}
		}
		StringBuilder result = new StringBuilder(xml.length() + 32);
		result.append("<svg width=\"").append(width);
		result.append("px\" height=\"").append(height).append("px\" ");
		result.append(xml, 5, xml.length());
		return result.toString().getBytes(StandardCharsets.UTF_8);
	}
	
	/**
	 * Change a signature JSON from PrimeFaces signature component to a PNG byte array
	 * @param json	The JSON from the Primefaces Signature component
	 * @param width	The required width of the resulting image
	 * @param height	The required height of the resulting image
	 * @param rgbHexBackgroundColour	The background colour - null is transparent
	 * @param rgbHexForegroundColour	The foreground colour - null is black
	 * @return	The PNG as bytes
	 * @throws IOException
	 */
	public static @Nonnull byte[] signature(@Nonnull String json,
												int width,
												int height,
												@Nullable String rgbHexBackgroundColour,
												@Nullable String rgbHexForegroundColour)
	throws IOException {
		List<List<Point>> lines = new ArrayList<>();
		Matcher lineMatcher = Pattern.compile("(\\[(?:,?\\[-?[\\d\\.]+,-?[\\d\\.]+\\])++\\])").matcher(json);
		while (lineMatcher.find()) {
			Matcher pointMatcher = Pattern.compile("\\[(-?[\\d\\.]+),(-?[\\d\\.]+)\\]").matcher(lineMatcher.group(1));
			List<Point> line = new ArrayList<>();
			lines.add(line);
			while (pointMatcher.find()) {
				line.add(new Point(Math.round(Float.parseFloat(pointMatcher.group(1))),
									Math.round(Float.parseFloat(pointMatcher.group(2)))));
			}
		}

		BufferedImage image = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB);
		Graphics2D g = (Graphics2D) image.getGraphics();
		
		g.setColor((rgbHexBackgroundColour == null) ? TRANSPARENT : Color.decode(rgbHexBackgroundColour));
		g.fillRect(0, 0, width, height);
		
		g.setColor((rgbHexForegroundColour == null) ? Color.BLACK : Color.decode(rgbHexForegroundColour));
		g.setStroke(new BasicStroke(2, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND));
		g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
		Point lastPoint = null;
		for (List<Point> line : lines) {
			for (Point point : line) {
				if (lastPoint != null) {
					g.drawLine(lastPoint.x, lastPoint.y, point.x, point.y);
				}
				lastPoint = point;
			}
			lastPoint = null;
		}
		
		try (ByteArrayOutputStream output = new ByteArrayOutputStream()) {
			ImageIO.write(image, ImageFormat.png.toString(), output); 
			return output.toByteArray();
		}
	}
	
	/**
	 * Walk the SVG DOM and replace "transparent" attribute values with "none", which is the
	 * equivalent that Batik's older CSS engine accepts. Handles both presentation attributes
	 * (e.g. fill="transparent") and inline style properties (e.g. style="stroke: transparent;").
	 */
	private static void normaliseTransparentForBatik(@Nonnull NodeList nodes) {
		for (int i = 0; i < nodes.getLength(); i++) {
			Node node = nodes.item(i);
			NamedNodeMap attrs = node.getAttributes();
			if (attrs != null) {
				for (int j = 0; j < attrs.getLength(); j++) {
					Node attr = attrs.item(j);
					String name = attr.getLocalName();
					String value = attr.getNodeValue();
					if ("style".equals(name)) {
						// Replace transparent in CSS property values, e.g. "fill: transparent; stroke: transparent"
						attr.setNodeValue(value.replaceAll("([:,])(\\s*)transparent\\b", "$1$2none"));
					}
					else if ("transparent".equals(value)) {
						// Presentation attribute, e.g. fill="transparent"
						attr.setNodeValue("none");
					}
				}
			}
			normaliseTransparentForBatik(node.getChildNodes());
		}
	}

	private static final String DOMINANT_BASELINE = "dominant-baseline";
	private static final String TEXT_BEFORE_EDGE = "text-before-edge";
	private static final float DEFAULT_FONT_SIZE = 16F;
	private static final FontRenderContext FONT_RENDER_CONTEXT = new FontRenderContext(null, true, true);

	/**
	 * Batik ignores dominant-baseline="text-before-edge", which Excalidraw uses to hang text from its top edge,
	 * and draws the text from its baseline at that point instead - a line too high.
	 * Move each such text element down by its font's ascent and let it sit on its baseline.
	 * 
	 * @param nodes	The nodes to normalise, recursively.
	 */
	private static void normaliseTextBeforeEdgeForBatik(@Nonnull NodeList nodes) {
		for (int i = 0; i < nodes.getLength(); i++) {
			Node node = nodes.item(i);
			if ((node instanceof Element element) &&
					"text".equals(element.getLocalName()) &&
					TEXT_BEFORE_EDGE.equals(element.getAttribute(DOMINANT_BASELINE).trim())) {
				hangTextFromItsBaseline(element);
			}
			normaliseTextBeforeEdgeForBatik(node.getChildNodes());
		}
	}

	/**
	 * Moves a text element hung from its top edge down by its font's ascent, so it can be drawn from its baseline.
	 * A text element positioned by a list of y values is left as it is.
	 */
	private static void hangTextFromItsBaseline(@Nonnull Element element) {
		float y = 0F;
		try {
			String value = element.getAttribute("y").trim();
			if (! value.isEmpty()) {
				y = Float.parseFloat(value);
			}
		}
		catch (@SuppressWarnings("unused") NumberFormatException e) {
			return; // a list of positions - leave it be
		}
		float fontSize = inheritedFontSize(element);
		String family = element.getAttribute("font-family");
		int comma = family.indexOf(',');
		family = ((comma < 0) ? family : family.substring(0, comma)).replace("\"", "").replace("'", "").trim();
		// Unknown families fall back to Java's default font, as they do in Batik
		Font font = new Font(family.isEmpty() ? Font.SANS_SERIF : family, Font.PLAIN, 1);
		float ascent = font.getLineMetrics("Hg", FONT_RENDER_CONTEXT).getAscent() * fontSize;
		element.setAttribute("y", Float.toString(y + ascent));
		element.removeAttribute(DOMINANT_BASELINE);
	}

	/**
	 * Returns the font size in pixels of the element given, from it or its nearest ancestor that sets one.
	 */
	private static float inheritedFontSize(@Nonnull Element element) {
		Node node = element;
		while (node instanceof Element ancestor) {
			String value = ancestor.getAttribute("font-size").trim();
			if (value.endsWith("px")) {
				value = value.substring(0, value.length() - 2).trim();
			}
			if (! value.isEmpty()) {
				try {
					return Float.parseFloat(value);
				}
				catch (@SuppressWarnings("unused") NumberFormatException e) {
					// try further up
				}
			}
			node = ancestor.getParentNode();
		}
		return DEFAULT_FONT_SIZE;
	}

	/**
	 * Batik Transcoder used in burnSvg().
	 */
	@SuppressWarnings("java:S110") // Batik's ImageTranscoder hierarchy exceeds 5 parents; not in our control
	private static class BufferedImageTranscoder extends ImageTranscoder {
		private BufferedImage image = null;

		private BufferedImageTranscoder(@Nonnull BufferedImage image) {
			this.image = image;
		}

		@Override
		public @Nonnull BufferedImage createImage(int w, int h) {
			return image;
		}

		@Override
		public void writeImage(@SuppressWarnings({"hiding", "null"}) @Nonnull BufferedImage image, @Nullable TranscoderOutput output) {
			this.image = image;
		}
	}

	/**
	 * An image dimension as a Batik transcoding hint.
	 */
	@SuppressWarnings("java:S9395") // image dimensions are far below 2^24, which a float holds exactly
	private static @Nonnull Float transcodingSize(int pixels) {
		return Float.valueOf(pixels);
	}

	/**
	 * Render the SVG given onto the background image given.
	 * <p>
	 * The SVG is scaled to cover the image, so markup drawn against the full resolution image
	 * (with a matching viewBox) lines up on a sub-sampled copy of it too.
	 * Markup whose width and height already match the image is drawn at its own size.
	 *
	 * @param image	The background image
	 * @param svg	The SVG markup to burn in.
	 */
	public static void burnSvg(@Nonnull BufferedImage image, @Nonnull String svg)
	throws ParserConfigurationException, SAXException, IOException, TransformerException, TranscoderException {
		BufferedImageTranscoder imageTranscoder = new BufferedImageTranscoder(image);
		imageTranscoder.addTranscodingHint(SVGAbstractTranscoder.KEY_WIDTH, transcodingSize(image.getWidth()));
		imageTranscoder.addTranscodingHint(SVGAbstractTranscoder.KEY_HEIGHT, transcodingSize(image.getHeight()));

		// Strip <style> elements before parsing - Batik requires xml-apis-ext to load
		// SVGOMStyleElement, but font-face declarations embedded by Excalidraw are not
		// needed for rendering the vector shapes.
		String svgForBatik = svg.replaceAll("(?s)<style[^>]*>.*?</style>", "");

		// Batik uses an old SVG/CSS spec that does not recognise "transparent" as a paint value.
		// Parse the SVG as DOM and replace transparent attribute values and inline style uses
		// with "none", which is the correct SVG 1.1 equivalent, before handing to Batik.
		@SuppressWarnings("java:S2755") // fixed with feature secure processing below
		DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
		factory.setFeature(XMLConstants.FEATURE_SECURE_PROCESSING, true);
		factory.setNamespaceAware(true);
		org.w3c.dom.Document doc = factory.newDocumentBuilder().parse(new InputSource(new StringReader(svgForBatik)));
		normaliseTransparentForBatik(doc.getChildNodes());
		normaliseTextBeforeEdgeForBatik(doc.getChildNodes());
		@SuppressWarnings("java:S2755") // fixed with feature secure processing below
		Transformer transformer = TransformerFactory.newInstance().newTransformer();
		factory.setFeature(XMLConstants.FEATURE_SECURE_PROCESSING, true);
		transformer.setOutputProperty(OutputKeys.INDENT, "no");
		StringWriter sw = new StringWriter(svgForBatik.length());
		transformer.transform(new DOMSource(doc), new StreamResult(sw));
		try (Writer w = sw) {
			svgForBatik = w.toString();
		}

		ByteArrayInputStream bais = new ByteArrayInputStream(svgForBatik.getBytes(StandardCharsets.UTF_8));
		TranscoderInput input = new TranscoderInput(bais);
		imageTranscoder.transcode(input, null);
	}
}
