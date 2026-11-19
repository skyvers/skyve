package org.skyve.content;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.junit.jupiter.api.Test;

@SuppressWarnings({ "static-method", "java:S8692" }) // system clock OK
class SearchResultTest {

	// ---- SearchResult ----

	@Test
	void testSearchResultDefaults() {
		SearchResult r = new SearchResult();
		assertNull(r.getContentId());
		assertNull(r.getExcerpt());
		assertEquals(0, r.getScore());
		assertNull(r.getCustomerName());
		assertNull(r.getModuleName());
		assertNull(r.getDocumentName());
		assertNull(r.getBizDataGroupId());
		assertNull(r.getBizUserId());
		assertNull(r.getBizId());
		assertNull(r.getAttributeName());
		assertNull(r.getLastModified());
		assertFalse(r.isAttachment(), "isAttachment() must be false when attributeName is null");
	}

	@Test
	void testSearchResultSettersAndGetters() {
		SearchResult r = new SearchResult();

		r.setContentId("cid-001");
		assertEquals("cid-001", r.getContentId());

		r.setExcerpt("this is an excerpt");
		assertEquals("this is an excerpt", r.getExcerpt());

		r.setScore(42);
		assertEquals(42, r.getScore());

		r.setCustomerName("demo");
		assertEquals("demo", r.getCustomerName());

		r.setModuleName("admin");
		assertEquals("admin", r.getModuleName());

		r.setDocumentName("User");
		assertEquals("User", r.getDocumentName());

		r.setBizDataGroupId("dg-1");
		assertEquals("dg-1", r.getBizDataGroupId());

		r.setBizUserId("user-1");
		assertEquals("user-1", r.getBizUserId());

		r.setBizId("biz-1");
		assertEquals("biz-1", r.getBizId());

		r.setAttributeName("photo");
		assertEquals("photo", r.getAttributeName());
		assertTrue(r.isAttachment(), "isAttachment() must be true when attributeName is set");

		Date now = new Date();
		r.setLastModified(now);
		assertEquals(now, r.getLastModified());
	}

	@Test
	void testIsAttachmentFalseWhenAttributeNameNull() {
		SearchResult r = new SearchResult();
		r.setAttributeName(null);
		assertFalse(r.isAttachment());
	}

	@Test
	void testIsAttachmentTrueWhenAttributeNameSet() {
		SearchResult r = new SearchResult();
		r.setAttributeName("file");
		assertTrue(r.isAttachment());
	}

	// ---- SearchResults ----

	@Test
	void testSearchResultsDefaults() {
		SearchResults sr = new SearchResults();
		assertNull(sr.getSearchTimeInSecs());
		assertNull(sr.getSuggestion());
		assertNotNull(sr.getResults());
		assertTrue(sr.getResults().isEmpty());
	}

	@Test
	void testSearchResultsSettersAndGetters() {
		SearchResults sr = new SearchResults();

		sr.setSearchTimeInSecs("0.005");
		assertEquals("0.005", sr.getSearchTimeInSecs());

		sr.setSuggestion("skyve");
		assertEquals("skyve", sr.getSuggestion());

		SearchResult r = new SearchResult();
		sr.getResults().add(r);
		assertEquals(1, sr.getResults().size());

		List<SearchResult> newList = new ArrayList<>();
		sr.setResults(newList);
		assertSame(newList, sr.getResults());
	}
}
