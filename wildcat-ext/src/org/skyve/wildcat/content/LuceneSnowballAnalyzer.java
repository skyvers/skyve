package org.skyve.wildcat.content;

import org.apache.lucene.analysis.snowball.SnowballAnalyzer;

public class LuceneSnowballAnalyzer extends SnowballAnalyzer {
	public LuceneSnowballAnalyzer() {
		super("English");
	}
}
