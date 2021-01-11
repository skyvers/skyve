package org.skyve.impl.content;

import java.io.ByteArrayInputStream;

import org.pf4j.Extension;
import org.apache.tika.Tika;
import org.skyve.content.TextExtractor;

@Extension(points = {TextExtractor.class})
public class TextExtractorImpl implements TextExtractor {
	private static final Tika TIKA = new Tika();
	
	@Override
	public String extractTextFromMarkup(String markup) throws Exception {
		return TIKA.parseToString(new ByteArrayInputStream(markup.getBytes()));
	}
}
