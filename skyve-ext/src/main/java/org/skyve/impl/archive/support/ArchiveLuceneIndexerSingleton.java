package org.skyve.impl.archive.support;

import java.io.IOException;
import java.nio.file.Path;
import java.util.List;
import java.util.Map.Entry;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;

import org.apache.lucene.analysis.Analyzer;
import org.apache.lucene.analysis.core.KeywordTokenizerFactory;
import org.apache.lucene.analysis.core.LowerCaseFilterFactory;
import org.apache.lucene.analysis.custom.CustomAnalyzer;
import org.apache.lucene.index.IndexWriter;
import org.apache.lucene.index.IndexWriterConfig;
import org.apache.lucene.store.Directory;
import org.apache.lucene.store.FSDirectory;
import org.skyve.impl.util.SystemObserver;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.UtilImpl.ArchiveConfig.ArchiveDocConfig;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.event.Level;

/**
 * A singleton class that manages Lucene indexers for document archiving operations.
 * Implements the SystemObserver interface to initialize indices on startup
 * and properly close resources on shutdown.
 */
public class ArchiveLuceneIndexerSingleton implements SystemObserver {
	private static final Logger LOGGER = LoggerFactory.getLogger(ArchiveLuceneIndexerSingleton.class);

	/** The singleton instance */
	private static ArchiveLuceneIndexerSingleton instance = new ArchiveLuceneIndexerSingleton();

	/** A concurrent map that maintains all active Lucene configurations */
	private final ConcurrentHashMap<ArchiveDocConfig, LuceneConfig> luceneConfigs = new ConcurrentHashMap<>();

	/**
	 * Private constructor to enforce singleton pattern.
	 */
	private ArchiveLuceneIndexerSingleton() {
		// disallow instantiation
	}

	/**
	 * Gets the singleton instance.
	 * 
	 * @return The singleton instance of ArchiveLuceneIndexerSingleton
	 */
	public static ArchiveLuceneIndexerSingleton getInstance() {
		return instance;
	}

	/**
	 * Initializes Lucene indexers for each configured archive document type.
	 * Called during system startup.
	 */
	@SuppressWarnings("resource")
	@Override
	public void startup() {

		List<ArchiveDocConfig> docConfigs = UtilImpl.ARCHIVE_CONFIG.docConfigs();

		for (ArchiveDocConfig docConfig : docConfigs) {
			Path indexDir = docConfig.getIndexDirectory();
			// Create IndexWriter for each ArchiveDocConfig
			try {
				Analyzer analyzer = newAnalyzer();
				Directory indexDirectory = FSDirectory.open(indexDir);
				IndexWriterConfig config = new IndexWriterConfig(analyzer);
				IndexWriter iwriter = new IndexWriter(indexDirectory, config);
				iwriter.commit();

				luceneConfigs.put(docConfig, new LuceneConfig(iwriter, indexDirectory));

			} catch (IOException e) {
				LOGGER.atLevel(Level.ERROR)
						.log("Failed to initialize Lucene index for: " + indexDir, e);
			}

		}

	}

	/**
	 * Properly closes all open index writers and directories.
	 * Called during system shutdown.
	 */
	@Override
	public void shutdown() {
		LOGGER.info("Shutting down Lucene Indexers...");

		for (Entry<ArchiveDocConfig, LuceneConfig> entry : luceneConfigs.entrySet()) {
			ArchiveDocConfig config = entry.getKey();
			LuceneConfig luceneConfig = entry.getValue();

			try {
				luceneConfig.indexWriter()
						.close();
				luceneConfig.indexDirectory()
						.close();
				LOGGER.info("Closed index for: " + config.getIndexDirectory());
			} catch (IOException e) {
				LOGGER.warn("Error closing Lucene index for: " + config.getIndexDirectory(), e);
			}
		}
		luceneConfigs.clear();

		LOGGER.info("Lucene Indexers shutdown complete.");

	}

	/**
	 * A record that stores an IndexWriter and its associated Directory.
	 */
	public static record LuceneConfig(IndexWriter indexWriter, Directory indexDirectory) {
	}

	/**
	 * Registers a new index configuration.
	 * 
	 * @param config The archive document configuration
	 * @param writer The index writer to add
	 * @param directory The directory associated with the index writer
	 */
	public void addIndexWriter(ArchiveDocConfig config, IndexWriter writer, Directory directory) {
		luceneConfigs.put(config, new LuceneConfig(writer, directory));
	}

	/**
	 * Retrieves the IndexWriter for a given configuration.
	 * 
	 * @param config The archive document configuration
	 * @return The associated IndexWriter, or null if not found
	 */
	public IndexWriter getIndexWriter(ArchiveDocConfig config) {
		return Optional.ofNullable(luceneConfigs.get(config))
				.map(LuceneConfig::indexWriter)
				.orElse(null);
	}

	/**
	 * Removes and closes resources for a configuration.
	 * 
	 * @param config The archive document configuration to remove
	 */
	public void removeIndexWriter(ArchiveDocConfig config) {
		LuceneConfig configToRemove = luceneConfigs.remove(config);
		if (configToRemove != null) {
			try {
				configToRemove.indexWriter()
						.close();
				configToRemove.indexDirectory()
						.close();
			} catch (Exception e) {
				LOGGER.warn("Error closing Lucene index for: " + config.getIndexDirectory(), e);
			}
		}
	}

	/**
	 * Checks if a configuration has an active index writer.
	 * 
	 * @param config The archive document configuration to check
	 * @return true if the configuration has an active index writer, false otherwise
	 */
	public boolean hasIndexWriter(ArchiveDocConfig config) {
		return luceneConfigs.containsKey(config);
	}

	/**
	 * Returns the entire configuration map.
	 * 
	 * @return The map of all active Lucene configurations
	 */
	public ConcurrentHashMap<ArchiveDocConfig, LuceneConfig> getLuceneConfigs() {
		return luceneConfigs;
	}

	/**
	 * Creates a custom Lucene analyzer using KeywordTokenizer and LowerCaseFilter.
	 * 
	 * @return A new analyzer instance
	 * @throws RuntimeException if analyzer creation fails
	 */
	private static Analyzer newAnalyzer() {
		try {
			return CustomAnalyzer.builder()
					.addTokenFilter(LowerCaseFilterFactory.NAME)
					.withTokenizer(KeywordTokenizerFactory.NAME)
					.build();
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
	}
}
