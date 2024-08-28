package org.skyve.impl.archive.support;

import static java.util.Collections.emptyList;
import static java.util.stream.Collectors.joining;
import static org.skyve.impl.archive.support.ArchiveUtils.ARCHIVE_CHARSET;

import java.io.IOException;
import java.io.RandomAccessFile;
import java.io.Serializable;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.stream.Stream;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.lucene.document.Document;
import org.apache.lucene.index.DirectoryReader;
import org.apache.lucene.search.IndexSearcher;
import org.apache.lucene.search.ScoreDoc;
import org.apache.lucene.search.TopDocs;
import org.apache.lucene.store.Directory;
import org.apache.lucene.store.FSDirectory;
import org.ehcache.Cache;
import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.cache.ArchivedDocumentCacheConfig;
import org.skyve.cache.Caching;
import org.skyve.domain.Bean;
import org.skyve.impl.archive.job.IndexArchivesJob;
import org.skyve.impl.archive.list.LuceneFilter;
import org.skyve.impl.util.UtilImpl.ArchiveConfig.ArchiveDocConfig;
import org.skyve.util.JSON;
import org.skyve.util.Util;

/**
 * Utility class for retrieving Beans archived to file.
 */
public class ArchiveRetriever {

    private final Logger logger = LogManager.getLogger();

    private static final String READ_ONLY = "r";

    private static final class SingletonHolder {
        private static final ArchiveRetriever INSTANCE = new ArchiveRetriever();
    }

    private ArchiveRetriever() {

    }

    public static ArchiveRetriever getInstance() {
        return SingletonHolder.INSTANCE;
    }

    /**
     * Retrieve 0 or 1 archived beans via that <em>Bean</em>'s bizId. If the bean has
     * been archived more than once which instance is returned is not defined (though
     * they should be identical). Requires the Bean's bizId to have been stored in a
     * field called "bizId".
     * 
     * @param bizId bizId of the archived bean to retrieve
     * @return an Optional containing the requested Bean, or an empty optional
     *         if it could not be found.
     */
    public <T extends Bean> Optional<T> retrieveByBizId(ArchiveDocConfig docConfig, String bizId) {

        try {
            LuceneFilter filter = new LuceneFilter();
            filter.addEquals(Bean.DOCUMENT_ID, bizId);

            List<ArchiveEntry> entries = searchIndex(docConfig, filter, 1);
            if (entries.isEmpty()) {
                return Optional.empty();
            }

            ArchiveEntry entry = entries.get(0);
            return Optional.ofNullable(retrieveBean(entry));
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * Retrive all archived beans that match the supplied <em>LuceneFilter</em>.
     * 
     * @param filter Filter to use to search the index
     * @return a list of results (empty if none are found)
     */
    public <T extends Bean> List<T> retrieveAll(ArchiveDocConfig docConfig, LuceneFilter filter, int maxResults) {

        try {
            List<ArchiveEntry> entries = searchIndex(docConfig, filter, maxResults);

            List<T> beans = new ArrayList<>(entries.size());
            for (ArchiveEntry entry : entries) {
                T a = retrieveBean(entry);
                beans.add(a);
            }

            return beans;
        } catch (IOException e) {
            throw new RuntimeException(e);
        }

    }

    /**
     * Search the archive index using the provided filter, returning the file
     * and offset where each result record can be found; or an empty list
     * if nothing is found.
     * 
     * @param bizId
     * @return
     * @throws IOException
     */
    private List<ArchiveEntry> searchIndex(ArchiveDocConfig docConfig, LuceneFilter filter, int maxResults) throws IOException {

        Path auditArchiveIndexPath = docConfig.getIndexDirectory();
        logger.debug("Searching for {}; using index at {}", filter, auditArchiveIndexPath);

        try (Directory directory = FSDirectory.open(auditArchiveIndexPath);
                DirectoryReader ireader = DirectoryReader.open(directory)) {

            IndexSearcher isearcher = new IndexSearcher(ireader);
            TopDocs td = isearcher.search(filter.toQuery(), maxResults);

            if (td.scoreDocs.length < 1) {
                logger.debug("No index entries found for {}", filter);
                return emptyList();
            }

            List<ArchiveEntry> entries = new ArrayList<>();

            for (ScoreDoc sd : td.scoreDocs) {

                int docID = sd.doc;
                Document doc = ireader.storedFields()
                                      .document(docID);

                long offset = doc.getField(IndexArchivesJob.OFFSET_FIELD)
                                 .numericValue()
                                 .longValue();
                long length = doc.getField(IndexArchivesJob.LENGTH_FIELD)
                                 .numericValue()
                                 .longValue();
                String fileName = doc.get(IndexArchivesJob.FILENAME_FIELD);

                ArchiveEntry entry = new ArchiveEntry(docConfig, fileName, offset, length);
                entries.add(entry);
            }

            logger.debug("Found {} results for {}", entries.size(), filter);
            return entries;
        }
    }

    @SuppressWarnings("unchecked")
    private <T extends Bean> T retrieveBean(ArchiveEntry entry) {

        Cache<Serializable, Bean> cache = getCache();

        Bean cachedBean = cache.get(entry.cacheKey());
        if (cachedBean != null) {
            logger.trace("Returning cached bean {} for {}", cachedBean, entry.cacheKey());
            return (T) cachedBean;
        }

        Bean bean = loadBeanFromFile(entry);
        cache.put(entry.cacheKey(), bean);

        return (T) bean;
    }

    private synchronized Cache<Serializable, Bean> getCache() {
        Caching caching = EXT.getCaching();
        Cache<Serializable, Bean> cache = caching.getEHCache(ArchivedDocumentCacheConfig.CACHE_NAME, Serializable.class,
                Bean.class);

        if (cache == null) {
            cache = caching.createEHCache(Util.getArchiveConfig()
                                              .cacheConfig());
            logger.debug("Created cache {}", cache);
        }

        return cache;
    }

    @SuppressWarnings("unchecked")
    private <T extends Bean> T loadBeanFromFile(ArchiveEntry entry) {
        try {
            Path archiveFilePath = getArchiveFilePath(entry);
            String line = readLine(archiveFilePath, entry.offset(), entry.length());
            T bean = (T) JSON.unmarshall(CORE.getUser(), line);
            bean.originalValues()
                .clear();
            return bean;
        } catch (Exception e) {
            logger.atFatal()
                  .withThrowable(e)
                  .log("Unable to retrieve archived entry {}", entry);
            throw new RuntimeException(e);
        }
    }

    /**
     * Read from the given file, starting at the provided offset, reading in the given
     * number of bytes. Converting the results to a String (utf8) for returning.
     */
    private String readLine(Path filePath, long offset, long length) throws IOException {

        logger.trace("Retrieving {} bytes from {} at {}", length, filePath, offset);

        // If this throws an ArithmeticException this record is longer than Integer.MAX_VALUE (2GB)
        // and can't be handled with a plain old byte array. The data on disk and in the index is
        // fine, but this method isn't written to handle that much data and will need to be updated
        // This is unlikely to ever be an issue, as a string is capped at Integer.MAX_VALUE bytes
        // anyway
        int lengthAsInt = Math.toIntExact(length);

        try (RandomAccessFile raf = new RandomAccessFile(filePath.toFile(), READ_ONLY)) {

            raf.seek(offset);
            byte[] bytes = new byte[lengthAsInt];
            raf.readFully(bytes);

            return new String(bytes, ARCHIVE_CHARSET);
        }
    }

    private Path getArchiveFilePath(ArchiveEntry entry) {

        Path p = entry.docConfig()
                      .getArchiveDirectory()
                      .resolve(entry.fileName());

        if (Files.isRegularFile(p)) {
            return p;
        }

        throw new IllegalArgumentException("Archive file path '" + p + "' is not valid");
    }

    private record ArchiveEntry(ArchiveDocConfig docConfig, String fileName, long offset, long length) {

        public String cacheKey() {

            return Stream.of(docConfig.module(), docConfig.document(), fileName, offset, length)
                         .map(String::valueOf)
                         .collect(joining("-"));
        }
    }
}
