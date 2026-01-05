package org.skyve.impl.util;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.RenderingHints;
import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.StringReader;
import java.io.StringWriter;
import java.io.Writer;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.imageio.ImageIO;
import javax.imageio.ImageReadParam;
import javax.imageio.ImageReader;
import javax.imageio.stream.ImageInputStream;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.apache.batik.transcoder.TranscoderInput;
import org.apache.batik.transcoder.TranscoderOutput;
import org.apache.batik.transcoder.image.ImageTranscoder;
import org.apache.commons.io.FilenameUtils;
import org.skyve.CORE;
import org.skyve.metadata.model.document.DynamicImage.ImageFormat;
import org.skyve.metadata.repository.Repository;
import org.skyve.util.FileUtil;
import org.skyve.util.Util;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import net.coobird.thumbnailator.util.exif.ExifFilterUtils;
import net.coobird.thumbnailator.util.exif.ExifUtils;
import net.coobird.thumbnailator.util.exif.Orientation;

/**
 * Image Utility methods.
 */
public class ImageUtil {
	private static final int FIRST_IMAGE_INDEX = 0;
	
	private static final Color TRANSPARENT = new Color(0, 0, 0, 0);
	
	/**
	 * Just like javax.imageio.ImageIO.read() but will subs-sample pixels for large images
	 * to constrain memory usage and apply EXIF rotation.
	 * 
	 * @param is	input stream
	 * @param subsamplingMinimumTargetSize	If the images smallest dimension is twice or more this value
	 * 											the image will be sub-sampled as its loaded to conserve memory.
	 * @return	The buffered image.
	 * @throws IOException
	 */
	public static final @Nullable BufferedImage read(@Nonnull InputStream is, int subsamplingMinimumTargetSize)
	throws IOException {
		BufferedImage result = null;
		
		try (ImageInputStream iis = ImageIO.createImageInputStream(is)) {
			Iterator<ImageReader> i = ImageIO.getImageReaders(iis);
			if (i.hasNext()) {
				ImageReader ir = i.next();
				try {
					ir.setInput(iis);
					ImageReadParam irp = ir.getDefaultReadParam();
					int smallestDimension = Math.min(ir.getWidth(FIRST_IMAGE_INDEX), ir.getHeight(FIRST_IMAGE_INDEX));
					int subsampling = smallestDimension / subsamplingMinimumTargetSize;
					
					Orientation orientation = null;
					try {
						orientation = ExifUtils.getExifOrientation(ir, FIRST_IMAGE_INDEX);
					}
					catch (@SuppressWarnings("unused") Exception e) {
						// do nothing - could not get orientation information so just continue on
					}

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
	 * Return UTF-8 byte array for an SVG representing the type of the file.
	 * 
	 * @param fileName	The filename suffix is used to get the svg file type icon
	 * @param imageWidth	The requested width.
	 * @param imageHeight	The requested height.
	 * @return	A byte[] to stream to a client.
	 * @throws IOException
	 */
	// See https://github.com/dmhendricks/file-icon-vectors/ for icons in /skyve-war/src/main/java/resources/files/
	public static @Nonnull byte[] svg(@Nonnull String fileName, int imageWidth, int imageHeight)
	throws IOException {
		String suffix = Util.processStringValue(FilenameUtils.getExtension(fileName));
		Repository repository = CORE.getRepository();
		File svgFile = null;
		if (suffix == null) {
			svgFile = repository.findResourceFile("files/blank.svg", null, null);
		}
		else {
			suffix = suffix.toLowerCase();
			svgFile = repository.findResourceFile(String.format("files/%s.svg", suffix) , null, null);
			if (! svgFile.exists()) {
				svgFile = repository.findResourceFile("files/blank.svg", null, null);
			}
		}
		// Add the requested width and height to the SVG so that it can be scaled 
		// and keep its aspect ratio in the browser <img/>.
		String xml = FileUtil.string(svgFile);
		xml = String.format("<svg width=\"%dpx\" height=\"%dpx\" %s",
								Integer.valueOf(imageWidth),
								Integer.valueOf(imageHeight),
								xml.substring(5));
		return xml.getBytes(StandardCharsets.UTF_8);
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
		Matcher lineMatcher = Pattern.compile("(\\[(?:,?\\[-?[\\d\\.]+,-?[\\d\\.]+\\])+\\])").matcher(json);
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
	 * Ensure SVG-Edit svg is valid by
	 * 1) Making sure rx and ry attributes are present in ellipses. if one is missing, set it to the other.
	 * 2) Removing ANY attribute that has a value of "null".
	 * 
	 * @param svg	The SVG produced by SVG-Edit
	 * @return	The cleansed SVG
	 * @throws Exception
	 */
	public static @Nonnull String cleanseSVGEdit(@Nonnull String svg) throws Exception {
		DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
		factory.setNamespaceAware(true);
		org.w3c.dom.Document doc = factory.newDocumentBuilder().parse(new InputSource(new StringReader(svg)));
		cleanseNodes(doc.getChildNodes(), new TreeSet<>());

		Transformer transformer = TransformerFactory.newInstance().newTransformer();
		transformer.setOutputProperty(OutputKeys.INDENT, "no");
		StreamResult result = new StreamResult(new StringWriter(svg.length()));
		DOMSource source = new DOMSource(doc);
		transformer.transform(source, result);
		try (Writer w = result.getWriter()) {
			return w.toString();
		}
	}

	private static void cleanseNodes(@Nonnull NodeList nodes, @Nonnull Set<String> namesToRemove) {
		// iterate backwards so we can remove unwanted nodes
		for (int i = nodes.getLength() - 1; i >= 0; i--) {
			Node node = nodes.item(i);
			String localName = node.getLocalName();
			NamedNodeMap nodeMap = node.getAttributes();
			// Remove comment nodes
			// NB the "Layer 1" <title/> node needs to remain for svg-edit to be able 
			// to create editable shapes inside that layer - the <g/>
			if ((node.getNodeType() == Node.COMMENT_NODE)) {
				node.getParentNode().removeChild(node);
			}
			else {
				if (nodeMap != null) {
					// fix rx & ry
					if ("ellipse".equals(localName)) {
						Element ellipse = (Element) node;
						String rx = Util.processStringValue(ellipse.getAttribute("rx"));
						String ry = Util.processStringValue(ellipse.getAttribute("ry"));
						if ((rx == null) && (ry != null)) {
							ellipse.setAttribute("rx", ry);
						}
						else if ((rx != null) && (ry == null)) {
							ellipse.setAttribute("ry", rx);
						}
					}
	
					// fix "null" nodes
					namesToRemove.clear();
					for (int j = 0; j < nodeMap.getLength(); j++) {
						Node nodeAttr = nodeMap.item(j);
						String value = nodeAttr.getTextContent();
						if ("null".equals(value)) {
							namesToRemove.add(nodeAttr.getLocalName());
						}
					}
					for (String name : namesToRemove) {
						nodeMap.removeNamedItem(name);
					}
				}
				cleanseNodes(node.getChildNodes(), namesToRemove);
			}
		}
	}
	
	/**
	 * Batik Transcoder used in burnSvg().
	 */
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
	 * Render the SVG given onto the background image given.
	 * 
	 * @param image	The background image
	 * @param svg	The SVG markup to burn in.
	 */
	public static void burnSvg(@Nonnull BufferedImage image, @Nonnull String svg)
	throws Exception {
		BufferedImageTranscoder imageTranscoder = new BufferedImageTranscoder(image);

		ByteArrayInputStream bais = new ByteArrayInputStream(svg.getBytes());
		TranscoderInput input = new TranscoderInput(bais);
		imageTranscoder.transcode(input, null);
	}
}
