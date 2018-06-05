package org.skyve.impl.util;

import java.util.HashMap;

/**
 * Utility class for holding {@link PluralUtil#articleResults(String)} results.
 */
public class ArticleNode extends HashMap<Character, ArticleNode> {

	private static final long serialVersionUID = -6153539100113536509L;
	Data data;

	public ArticleNode() {
		data = new Data();
	}

	class Data {

		private Integer aCount = Integer.valueOf(0);
		private Integer anCount = Integer.valueOf(0);
		private String prefix;
		private String article;

		/**
		 * The number of times "a" was seen for this prefix, e.g. 9682
		 */
		public Integer getaCount() {
			return aCount;
		}

		public void setaCount(Integer aCount) {
			this.aCount = aCount;
		}

		/**
		 * The number of times "an" was seen for this prefix, e.g. 1028246
		 */
		public Integer getAnCount() {
			return anCount;
		}

		public void setAnCount(Integer anCount) {
			this.anCount = anCount;
		}

		/**
		 * The prefix sufficient to determine the article, e.g. "e"
		 */
		public String getPrefix() {
			return prefix;
		}

		public void setPrefix(String prefix) {
			this.prefix = prefix;
		}

		/**
		 * The most common article for the prefix, a or an. E.g. "an"
		 */
		public String getArticle() {
			return article;
		}

		public void setArticle(String article) {
			this.article = article;
		}

		@Override
		public String toString() {
			return String.format("Matching prefix: %s\nCorrect article: %s\na count: %d\nan count: %d",
					prefix,
					article,
					aCount,
					anCount);
		}
	}
}
