/**
 * List model and Lucene filter for browsing and searching archived documents.
 *
 * <p>{@code ArchivedDocumentListModel} is an abstract {@link org.skyve.metadata.view.model.list.ListModel}
 * that queries the archive's Lucene index to populate a list view of archived
 * document records. {@code LuceneFilter} translates Skyve list-filter operations
 * into Lucene {@link org.apache.lucene.search.Query} instances for index searches.
 *
 * @see org.skyve.impl.archive.support
 * @see org.skyve.impl.archive.job
 */
package org.skyve.impl.archive.list;
