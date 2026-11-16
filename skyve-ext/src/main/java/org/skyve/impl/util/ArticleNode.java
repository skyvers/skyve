package org.skyve.impl.util;

import java.util.HashMap;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

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
		public @Nonnull Integer getaCount() {
			return aCount;
		}

		public void setaCount(@Nonnull Integer aCount) {
			this.aCount = aCount;
		}

		/**
		 * The number of times "an" was seen for this prefix, e.g. 1028246
		 */
		public @Nonnull Integer getAnCount() {
			return anCount;
		}

		public void setAnCount(@Nonnull Integer anCount) {
			this.anCount = anCount;
		}

		/**
		 * The prefix sufficient to determine the article, e.g. "e"
		 */
		public @Nullable String getPrefix() {
			return prefix;
		}

		public void setPrefix(@Nonnull String prefix) {
			this.prefix = prefix;
		}

		/**
		 * The most common article for the prefix, a or an. E.g. "an"
		 */
		public @Nullable String getArticle() {
			return article;
		}

		public void setArticle(@Nonnull String article) {
			this.article = article;
		}

		@Override
		public @Nonnull String toString() {
			return String.format("Matching prefix: %s\nCorrect article: %s\na count: %d\nan count: %d",
					prefix,
					article,
					aCount,
					anCount);
		}
	}
}
