package org.skyve.impl.util;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.RenderingHints;
import java.awt.image.BufferedImage;
import java.io.BufferedInputStream;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.imageio.ImageIO;
import javax.imageio.ImageReadParam;
import javax.imageio.ImageReader;
import javax.imageio.stream.ImageInputStream;

import org.apache.commons.io.FilenameUtils;
import org.skyve.CORE;
import org.skyve.content.MimeType;
import org.skyve.metadata.model.document.DynamicImage.ImageFormat;
import org.skyve.metadata.repository.Repository;
import org.skyve.util.FileUtil;
import org.skyve.util.Util;

import net.coobird.thumbnailator.Thumbnails;
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
	public static final BufferedImage read(InputStream is, int subsamplingMinimumTargetSize) throws IOException {
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
	// See https://github.com/dmhendricks/file-icon-vectors/ for icons in /skyve-ee/src/skyve/resources/files/
	public static byte[] svg(String fileName, int imageWidth, int imageHeight) throws IOException {
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
		String xml = FileUtil.getFileAsString(svgFile);
		xml = String.format("<svg width=\"%dpx\" height=\"%dpx\" %s",
								Integer.valueOf(imageWidth),
								Integer.valueOf(imageHeight),
								xml.substring(5));
		return xml.getBytes(Util.UTF8);
	}
	
	/**
	 * Read the bytes from an image file.
	 * 
	 * @param file	The file to read.
	 * @return	the bytes.
	 * @throws IOException
	 */
	public static byte[] image(File file) throws IOException {
		try (FileInputStream fis = new FileInputStream(file)) {
			return image(fis);
		}
	}
	
	/**
	 * Read the bytes from an image InputStream.
	 * 
	 * @param is	The input stream to read.
	 * @return	the bytes.
	 * @throws IOException
	 */
	public static byte[] image(InputStream is) throws IOException {
		try (BufferedInputStream bis = new BufferedInputStream(is)) {
			try (ByteArrayOutputStream baos = new ByteArrayOutputStream()) {
				byte[] bytes = new byte[1024]; // 1K
				int bytesRead = 0;
				while ((bytesRead = bis.read(bytes)) > 0) {
					baos.write(bytes, 0, bytesRead);
				}
				return baos.toByteArray();
			}
		}
	}
	
	
	public static byte[] signature(String json,
									int width,
									int height,
									String rgbHexBackgroundColour,
									String rgbHexForegroundColour)
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
	
	public static byte[] reduceImageSize(MimeType mimeType, byte[] bytes) throws Exception {
		// Convert max image upload size from MegaBytes to Bytes (*1024^2)
		Integer maxImageSizeBytes = UtilImpl.UPLOADS_IMAGE_MAXIMUM_SIZE_IN_MB * 1048576;

		if (MimeType.jpeg.equals(mimeType) || MimeType.png.equals(mimeType)) {
			if (bytes.length > maxImageSizeBytes) {
				try (InputStream is = new ByteArrayInputStream(bytes)) {
					BufferedImage image = ImageUtil.read(is, UtilImpl.THUMBNAIL_SUBSAMPLING_MINIMUM_TARGET_SIZE);
					int width = image.getWidth();
					int height = image.getHeight();
					// change width and height to a fraction of it width and height somehow (keeping aspect ratio)
					image = Thumbnails.of(image).size(width, height).keepAspectRatio(true).asBufferedImage();
					try (ByteArrayOutputStream baos = new ByteArrayOutputStream()) {
						// calculate scale from image size and max image size in bytes
						double scale = bytes.length / (maxImageSizeBytes + 1);
						// Create the thumb nail
						Thumbnails.of(image).scale(scale).outputFormat(mimeType.getStandardFileSuffix()).toOutputStream(baos);
						return baos.toByteArray();
					}
				}
			}
		}
		return bytes;
	}
}
