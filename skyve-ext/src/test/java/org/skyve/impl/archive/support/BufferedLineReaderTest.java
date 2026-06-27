package org.skyve.impl.archive.support;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;

import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

/**
 * Tests for {@link BufferedLineReader}.
 */
public class BufferedLineReaderTest {

	@Rule
	public TemporaryFolder tmp = new TemporaryFolder();

	private Path writeFile(String content) throws IOException {
		Path path = tmp.newFile().toPath();
		Files.write(path, content.getBytes(StandardCharsets.UTF_8));
		return path;
	}

	@Test
	public void readSingleLine() throws IOException {
		Path path = writeFile("hello world\n");
		try (BufferedLineReader reader = new BufferedLineReader(path, 0)) {
			BufferedLineReader.Line line = reader.readLine();
			assertNotNull(line);
			assertEquals("hello world", line.line());
			assertNull(reader.readLine());
		}
	}

	@Test
	public void readMultipleLines() throws IOException {
		Path path = writeFile("line1\nline2\nline3\n");
		try (BufferedLineReader reader = new BufferedLineReader(path, 0)) {
			assertEquals("line1", reader.readLine().line());
			assertEquals("line2", reader.readLine().line());
			assertEquals("line3", reader.readLine().line());
			assertNull(reader.readLine());
		}
	}

	@Test
	public void readLineWithoutTrailingNewline() throws IOException {
		Path path = writeFile("no newline at end");
		try (BufferedLineReader reader = new BufferedLineReader(path, 0)) {
			BufferedLineReader.Line line = reader.readLine();
			assertNotNull(line);
			assertEquals("no newline at end", line.line());
			assertNull(reader.readLine());
		}
	}

	@Test
	public void readEmptyFile() throws IOException {
		Path path = writeFile("");
		try (BufferedLineReader reader = new BufferedLineReader(path, 0)) {
			assertNull(reader.readLine());
		}
	}

	@Test
	public void lineOffsetsAreCorrect() throws IOException {
		// "abc\ndef\n" — line1 starts at 0, ends at 4 (after \n); line2 starts at 4, ends at 8
		Path path = writeFile("abc\ndef\n");
		try (BufferedLineReader reader = new BufferedLineReader(path, 0)) {
			BufferedLineReader.Line line1 = reader.readLine();
			assertNotNull(line1);
			assertEquals(0L, line1.offset());
			assertEquals(4L, line1.end());

			BufferedLineReader.Line line2 = reader.readLine();
			assertNotNull(line2);
			assertEquals(4L, line2.offset());
			assertEquals(8L, line2.end());
		}
	}

	@Test
	public void lineLengthExcludesNewline() throws IOException {
		// "hello\n" — length = 5 (not including '\n')
		Path path = writeFile("hello\n");
		try (BufferedLineReader reader = new BufferedLineReader(path, 0)) {
			BufferedLineReader.Line line = reader.readLine();
			assertNotNull(line);
			assertEquals(5L, line.length());
		}
	}

	@Test
	public void startOffsetSkipsBytes() throws IOException {
		// "skip\nread\n" — start reading from offset 5 (skipping "skip\n")
		Path path = writeFile("skip\nread\n");
		try (BufferedLineReader reader = new BufferedLineReader(path, 5)) {
			BufferedLineReader.Line line = reader.readLine();
			assertNotNull(line);
			assertEquals("read", line.line());
		}
	}
}
