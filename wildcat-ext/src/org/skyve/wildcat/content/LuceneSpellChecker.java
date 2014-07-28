/*
 * Licensed to the Apache Software Foundation (ASF) under one or more contributor license agreements. See the NOTICE file
 * distributed with this work for additional information regarding copyright ownership. The ASF licenses this file to You under the
 * Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a
 * copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS"
 * BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language
 * governing permissions and limitations under the License.
 */
package org.skyve.wildcat.content;

import java.io.IOException;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.List;

import org.apache.jackrabbit.core.query.QueryConstants;
import org.apache.jackrabbit.core.query.QueryHandler;
import org.apache.jackrabbit.core.query.QueryRootNode;
import org.apache.jackrabbit.core.query.RelationQueryNode;
import org.apache.jackrabbit.core.query.TraversingQueryNodeVisitor;
import org.apache.jackrabbit.core.query.lucene.FieldNames;
import org.apache.jackrabbit.core.query.lucene.SearchIndex;
import org.apache.lucene.analysis.Token;
import org.apache.lucene.analysis.TokenStream;
import org.apache.lucene.index.IndexReader;
import org.apache.lucene.search.spell.Dictionary;
import org.apache.lucene.search.spell.LuceneDictionary;
import org.apache.lucene.search.spell.SpellChecker;
import org.apache.lucene.store.AlreadyClosedException;
import org.apache.lucene.store.Directory;
import org.apache.lucene.store.FSDirectory;
import org.apache.lucene.store.NativeFSLockFactory;

/**
 * <code>LuceneSpellChecker</code> implements a spell checker based on the terms present in a lucene index.
 */
public class LuceneSpellChecker implements org.apache.jackrabbit.core.query.lucene.SpellChecker {
	public static final class FiveSecondsRefreshInterval extends LuceneSpellChecker {
		public FiveSecondsRefreshInterval() {
			super(5 * 1000);
		}
	}

	public static final class OneMinuteRefreshInterval extends LuceneSpellChecker {
		public OneMinuteRefreshInterval() {
			super(60 * 1000);
		}
	}

	public static final class FiveMinutesRefreshInterval extends LuceneSpellChecker {
		public FiveMinutesRefreshInterval() {
			super(5 * 60 * 1000);
		}
	}

	public static final class ThirtyMinutesRefreshInterval extends LuceneSpellChecker {
		public ThirtyMinutesRefreshInterval() {
			super(30 * 60 * 1000);
		}
	}

	public static final class OneHourRefreshInterval extends LuceneSpellChecker {
		public OneHourRefreshInterval() {
			super(60 * 60 * 1000);
		}
	}

	public static final class SixHoursRefreshInterval extends LuceneSpellChecker {
		public SixHoursRefreshInterval() {
			super(6 * 60 * 60 * 1000);
		}
	}

	public static final class TwelveHoursRefreshInterval extends LuceneSpellChecker {
		public TwelveHoursRefreshInterval() {
			super(12 * 60 * 60 * 1000);
		}
	}

	public static final class OneDayRefreshInterval extends LuceneSpellChecker {
		public OneDayRefreshInterval() {
			super(24 * 60 * 60 * 1000);
		}
	}

	/**
	 * The internal spell checker.
	 */
	private InternalSpellChecker spellChecker;

	/**
	 * The refresh interval.
	 */
	private final long refreshInterval;

	/**
	 * Spell checker with a default refresh interval of one hour.
	 */
	public LuceneSpellChecker() {
		this(60 * 60 * 1000); // default refresh interval: one hour
	}

	protected LuceneSpellChecker(long refreshInterval) {
		this.refreshInterval = refreshInterval;
	}

	/**
	 * Initializes this spell checker.
	 * 
	 * @param handler the query handler that created this spell checker.
	 * @throws IOException if <code>handler</code> is not of type {@link SearchIndex}.
	 */
	@Override
	public void init(QueryHandler handler) throws IOException {
		if (handler instanceof SearchIndex) {
			this.spellChecker = new InternalSpellChecker((SearchIndex) handler);
		}
		else {
			throw new IOException("LuceneSpellChecker only works with " + SearchIndex.class.getName());
		}
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public String check(QueryRootNode aqt) throws IOException {
		String stmt = getFulltextStatement(aqt);
		if (stmt == null) {
			// no spellcheck operation in query
			return null;
		}
		return spellChecker.suggest(stmt);
	}

	@Override
	public void close() {
		spellChecker.close();
	}

	// ------------------------------< internal >--------------------------------

	/**
	 * Returns the full text statement of a spell check relation query node or <code>null</code> 
	 * if none exists in the abstract query tree.
	 * 
	 * @param aqt the abstract query tree.
	 * @return the full text statement or <code>null</code>.
	 */
	private static String getFulltextStatement(QueryRootNode aqt) {
		final String[] stmt = new String[1];
		aqt.accept(new TraversingQueryNodeVisitor() {
			@Override
			public Object visit(RelationQueryNode node, Object o) {
				if (stmt[0] == null && node.getOperation() == QueryConstants.OPERATION_SPELLCHECK) {
					stmt[0] = node.getStringValue();
				}
				return super.visit(node, o);
			}
		}, null);
		return stmt[0];
	}

	private final class InternalSpellChecker {

		/**
		 * Timestamp when the last refresh was done.
		 */
		private long lastRefresh;

		/**
		 * Set to true while a refresh is done in a separate thread.
		 */
		private boolean refreshing = false;

		/**
		 * The query handler associated with this spell checker.
		 */
		private final SearchIndex handler;

		/**
		 * The directory where the spell index is stored.
		 */
		private final Directory spellIndexDirectory;

		/**
		 * The underlying spell checker.
		 */
		@SuppressWarnings("hiding")
		private SpellChecker spellChecker;

		/**
		 * Creates a new internal spell checker.
		 * 
		 * @param handler the associated query handler.
		 */
		InternalSpellChecker(SearchIndex handler) 
		throws IOException {
			this.handler = handler;
			String path = handler.getPath() + "/spellchecker";
			this.spellIndexDirectory = FSDirectory.getDirectory(path, new NativeFSLockFactory(path));
			if (IndexReader.indexExists(spellIndexDirectory)) {
				this.lastRefresh = System.currentTimeMillis();
			}
			this.spellChecker = new SpellChecker(spellIndexDirectory);
			refreshSpellChecker();
		}

		/**
		 * Checks a fulltext query statement and suggests a spell checked version of the statement. If the spell checker thinks the
		 * spelling is correct <code>null</code> is returned.
		 * 
		 * @param statement the fulltext query statement.
		 * @return a suggestion or <code>null</code>.
		 */
		String suggest(String statement) throws IOException {
			// tokenize the statement (field name doesn't matter actually...)
			List<String> words = new ArrayList<>();
			List<Token> tokens = new ArrayList<>();
			tokenize(statement, words, tokens);

			String[] suggestions = check(words.toArray(new String[words.size()]));
			if (suggestions != null) {
				// replace words in statement in reverse order because length of statement will change
				StringBuffer sb = new StringBuffer(statement);
				for (int i = suggestions.length - 1; i >= 0; i--) {
					Token t = tokens.get(i);
					// only replace if word acutally changed
					if (! t.termText().equalsIgnoreCase(suggestions[i])) {
						sb.replace(t.startOffset(), t.endOffset(), suggestions[i]);
					}
				}
				
				return sb.toString();
			}

			return null;
		}

		void close() {
			try {
				spellIndexDirectory.close();
			}
			catch (IOException e) {
				// ignore
			}
			// urgh, the lucene spell checker cannot be closed explicitly.
			// finalize will close the reader...
			spellChecker = null;
		}

		/**
		 * Tokenizes the statement into words and tokens.
		 * 
		 * @param statement the fulltext query statement.
		 * @param words this list will be filled with the original words extracted from the statement.
		 * @param tokens this list will be filled with the tokens parsed from the statement.
		 * @throws IOException if an error occurs while parsing the statement.
		 */
		private void tokenize(String statement, List<String> words, List<Token> tokens) 
		throws IOException {
			TokenStream ts = handler.getTextAnalyzer().tokenStream(FieldNames.FULLTEXT, new StringReader(statement));
			try {
				Token t;
				while ((t = ts.next()) != null) {
					String origWord = statement.substring(t.startOffset(), t.endOffset());
					if (t.getPositionIncrement() > 0) {
						words.add(t.termText());
						tokens.add(t);
					}
					else {
						// very simple implementation: use termText with length
						// closer to original word
						Token current = tokens.get(tokens.size() - 1);
						if (Math.abs(origWord.length() - current.termText().length()) > 
								Math.abs(origWord.length() - t.termText().length())) {
							// replace current token and word
							words.set(words.size() - 1, t.termText());
							tokens.set(tokens.size() - 1, t);
						}
					}
				}
			}
			finally {
				ts.close();
			}
		}

		/**
		 * Checks the spelling of the passed <code>words</code> and returns a suggestion.
		 * 
		 * @param words the words to check.
		 * @return a suggestion of correctly spelled <code>words</code> or <code>null</code> 
		 * 			if this spell checker thinks <code>words</code> are spelled correctly.
		 * @throws IOException if an error occurs while spell checking.
		 */
		private String[] check(String words[]) throws IOException {
			refreshSpellChecker();
			boolean hasSuggestion = false;
			IndexReader reader = handler.getIndexReader();
			try {
				for (int retries = 0; retries < 100; retries++) {
					try {
						String[] suggestion = new String[words.length];
						for (int i = 0; i < words.length; i++) {
							String[] similar = spellChecker.suggestSimilar(words[i], 
																			5, 
																			reader, 
																			FieldNames.FULLTEXT, 
																			true);
							if (similar.length > 0) {
								suggestion[i] = similar[0];
								hasSuggestion = true;
							}
							else {
								suggestion[i] = words[i];
							}
						}
						if (hasSuggestion) {
							return suggestion;
						}

						return null;
					}
					catch (AlreadyClosedException e) {
						// it may happen that the index reader inside the
						// spell checker is closed while searching for
						// suggestions. this is actually a design flaw in the
						// lucene spell checker, but for now we simply retry
					}
				}
				// unsuccessful after retries
				return null;
			}
			finally {
				reader.close();
			}
		}

		/**
		 * Refreshes the underlying spell checker in a background thread. Synchronization is done on this <code>LuceneSpellChecker</code> instance. 
		 * While the refresh takes place {@link #refreshing} is set to <code>true</code>.
		 */
		@SuppressWarnings("synthetic-access")
		private void refreshSpellChecker() {
			if (lastRefresh + refreshInterval < System.currentTimeMillis()) {
				synchronized (this) {
					if (refreshing) {
						return;
					}

					refreshing = true;
					Runnable refresh = new Runnable() {
						@Override
						public void run() {
							try {
								IndexReader reader = handler.getIndexReader();
								try {
									long time = System.currentTimeMillis();
									Dictionary dict = new LuceneDictionary(reader, FieldNames.FULLTEXT);
									spellChecker.indexDictionary(dict);
									time = System.currentTimeMillis() - time;
									time = time / 1000;
								}
								finally {
									reader.close();
									synchronized (InternalSpellChecker.this) {
										refreshing = false;
									}
								}
							}
							catch (IOException e) {
								// ignore
							}
						}
					};
					new Thread(refresh, "SpellChecker Refresh").start();
					lastRefresh = System.currentTimeMillis();
				}
			}
		}
	}
}
