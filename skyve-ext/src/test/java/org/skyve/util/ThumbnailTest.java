package org.skyve.util;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;

import javax.imageio.ImageIO;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.skyve.content.AttachmentContent;
import org.skyve.content.MimeType;

@SuppressWarnings("static-method")
class ThumbnailTest {

	@TempDir
	File tempDir;

	/** Writes a minimal 2×2 PNG to a temp file and returns it. */
	private File createPngFile() throws IOException {
		BufferedImage img = new BufferedImage(2, 2, BufferedImage.TYPE_INT_RGB);
		Graphics2D g = img.createGraphics();
		g.setColor(Color.RED);
		g.fillRect(0, 0, 2, 2);
		g.dispose();
		File png = new File(tempDir, "test.png");
		ImageIO.write(img, "png", png);
		return png;
	}

	/** Writes a small SVG text to a temp file and returns it. */
	private File createSvgFile() throws IOException {
		File svg = new File(tempDir, "test.svg");
		Files.write(svg.toPath(), "<svg xmlns=\"http://www.w3.org/2000/svg\"><rect width=\"10\" height=\"10\"/></svg>".getBytes());
		return svg;
	}

	/** Writes some arbitrary bytes to a temp file named *.txt and returns it. */
	private File createTextFile() throws IOException {
		File txt = new File(tempDir, "test.txt");
		Files.write(txt.toPath(), "hello world".getBytes());
		return txt;
	}

	@Test
	void fileConstructorReadsBytesFromPngFile() throws IOException {
		File pngFile = createPngFile();
		Thumbnail t = new Thumbnail(pngFile);
		assertNotNull(t.getBytes());
		// bytes should be the exact PNG content
		assertArrayEquals(Files.readAllBytes(pngFile.toPath()), t.getBytes());
	}

	@Test
	void fileConstructorDetectsPngMimeType() throws IOException {
		File pngFile = createPngFile();
		Thumbnail t = new Thumbnail(pngFile);
		assertEquals(MimeType.png, t.getMimeType());
	}

	@Test
	void fileConstructorReadsBytesFromSvgFile() throws IOException {
		File svgFile = createSvgFile();
		Thumbnail t = new Thumbnail(svgFile);
		assertNotNull(t.getBytes());
		assertArrayEquals(Files.readAllBytes(svgFile.toPath()), t.getBytes());
	}

	@Test
	void fileConstructorDetectsSvgMimeType() throws IOException {
		File svgFile = createSvgFile();
		Thumbnail t = new Thumbnail(svgFile);
		assertEquals(MimeType.svg, t.getMimeType());
	}

	@Test
	void fileConstructorNonImageFileHasMimeTypeOrNull() throws IOException {
		File txtFile = createTextFile();
		Thumbnail t = new Thumbnail(txtFile);
		assertNotNull(t.getBytes());
		// .txt may or may not have a known MimeType depending on Skyve's enum; just verify bytes are present
		assertArrayEquals(Files.readAllBytes(txtFile.toPath()), t.getBytes());
	}

	@Test
	void getBytesReturnsNonEmptyArrayForPng() throws IOException {
		File pngFile = createPngFile();
		Thumbnail t = new Thumbnail(pngFile);
		byte[] bytes = t.getBytes();
		assertNotNull(bytes);
		// A PNG always starts with the 8-byte signature
		assertEquals((byte) 0x89, bytes[0]);
		assertEquals((byte) 'P', bytes[1]);
		assertEquals((byte) 'N', bytes[2]);
		assertEquals((byte) 'G', bytes[3]);
	}

	// ---- AttachmentContent constructor (width=0, height=0) ----

	private AttachmentContent createPngContent() throws IOException {
		BufferedImage img = new BufferedImage(2, 2, BufferedImage.TYPE_INT_RGB);
		Graphics2D g = img.createGraphics();
		g.setColor(Color.BLUE);
		g.fillRect(0, 0, 2, 2);
		g.dispose();
		File pngFile = new File(tempDir, "content.png");
		ImageIO.write(img, "png", pngFile);
		byte[] pngBytes = Files.readAllBytes(pngFile.toPath());
		AttachmentContent content = new AttachmentContent("testCustomer", "testModule", "testDocument", null, "testUser", "testId", "testAttr");
		content.attachment("content.png", MimeType.png, pngBytes);
		return content;
	}

	@Test
	void contentConstructorZeroSizeReturnsBytesDirectly() throws IOException {
		AttachmentContent content = createPngContent();
		byte[] expected = content.getContentBytes();
		Thumbnail t = new Thumbnail(content);
		assertNotNull(t.getBytes());
		assertArrayEquals(expected, t.getBytes());
	}

	@Test
	void contentConstructorZeroSizeReturnsMimeType() throws IOException {
		AttachmentContent content = createPngContent();
		Thumbnail t = new Thumbnail(content);
		assertEquals(MimeType.png, t.getMimeType());
	}

	@Test
	void contentConstructorWithWidthHeightZeroReturnsBytesDirectly() throws IOException {
		AttachmentContent content = createPngContent();
		byte[] expected = content.getContentBytes();
		Thumbnail t = new Thumbnail(content, 0, 0);
		assertNotNull(t.getBytes());
		assertArrayEquals(expected, t.getBytes());
	}

	// ---- InputStream constructor ----

	@Test
	void inputStreamConstructorWithSvgContent() throws IOException {
		// SVG is returned directly without resizing or file storage
		byte[] svgBytes = "<svg xmlns=\"http://www.w3.org/2000/svg\"><rect width=\"10\" height=\"10\"/></svg>".getBytes();
		Thumbnail t = new Thumbnail(new ByteArrayInputStream(svgBytes), "/test/image.svg", "image/svg+xml", 64, 64);
		assertNotNull(t.getBytes());
		assertEquals(MimeType.svg, t.getMimeType());
	}

	@Test
	void getMimeTypeReturnsNullForUnknownMimeType() throws IOException {
		// File constructor with unknown extension returns null mimeType
		File unknownFile = new File(tempDir, "test.unknownext");
		Files.write(unknownFile.toPath(), "data".getBytes());
		Thumbnail t = new Thumbnail(unknownFile);
		// MimeType.fromFileName returns null for unknown extensions
		assertNull(t.getMimeType());
	}
}
