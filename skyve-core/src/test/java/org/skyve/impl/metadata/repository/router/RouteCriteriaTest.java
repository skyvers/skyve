package org.skyve.impl.metadata.repository.router;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.web.WebAction;

@SuppressWarnings("static-method")
class RouteCriteriaTest {

	@Test
	void defaultConstructorCreatesInstance() {
		assertNotNull(new RouteCriteria());
	}

	@Test
	void viewTypeRoundTrip() {
		RouteCriteria rc = new RouteCriteria();
		rc.setViewType(ViewType.edit);
		assertEquals(ViewType.edit, rc.getViewType());
	}

	@Test
	void viewTypeNullByDefault() {
		assertNull(new RouteCriteria().getViewType());
	}

	@Test
	void webActionRoundTrip() {
		RouteCriteria rc = new RouteCriteria();
		rc.setWebAction(WebAction.e);
		assertEquals(WebAction.e, rc.getWebAction());
	}

	@Test
	void moduleNameRoundTrip() {
		RouteCriteria rc = new RouteCriteria();
		rc.setModuleName("admin");
		assertEquals("admin", rc.getModuleName());
	}

	@Test
	void moduleNameBlankBecomesNull() {
		RouteCriteria rc = new RouteCriteria();
		rc.setModuleName("  ");
		assertNull(rc.getModuleName());
	}

	@Test
	void documentNameRoundTrip() {
		RouteCriteria rc = new RouteCriteria();
		rc.setDocumentName("Contact");
		assertEquals("Contact", rc.getDocumentName());
	}

	@Test
	void queryNameRoundTrip() {
		RouteCriteria rc = new RouteCriteria();
		rc.setQueryName("qContacts");
		assertEquals("qContacts", rc.getQueryName());
	}

	@Test
	void customerNameRoundTrip() {
		RouteCriteria rc = new RouteCriteria();
		rc.setCustomerName("demo");
		assertEquals("demo", rc.getCustomerName());
	}

	@Test
	void dataGroupIdRoundTrip() {
		RouteCriteria rc = new RouteCriteria();
		rc.setDataGroupId("dg1");
		assertEquals("dg1", rc.getDataGroupId());
	}

	@Test
	void userIdRoundTrip() {
		RouteCriteria rc = new RouteCriteria();
		rc.setUserId("user1");
		assertEquals("user1", rc.getUserId());
	}

	@Test
	void matchesReturnsTrueForEmptyCriteriaAgainstAnything() {
		RouteCriteria empty = new RouteCriteria();
		RouteCriteria other = new RouteCriteria();
		other.setModuleName("admin");
		other.setDocumentName("Contact");
		assertTrue(empty.matches(other));
	}

	@Test
	void matchesReturnsTrueWhenAllFieldsMatch() {
		RouteCriteria a = new RouteCriteria();
		a.setModuleName("admin");
		a.setDocumentName("Contact");
		a.setViewType(ViewType.edit);

		RouteCriteria b = new RouteCriteria();
		b.setModuleName("admin");
		b.setDocumentName("Contact");
		b.setViewType(ViewType.edit);

		assertTrue(a.matches(b));
	}

	@Test
	void matchesReturnsFalseWhenModuleNameDiffers() {
		RouteCriteria a = new RouteCriteria();
		a.setModuleName("admin");

		RouteCriteria b = new RouteCriteria();
		b.setModuleName("contacts");

		assertFalse(a.matches(b));
	}

	@Test
	void matchesReturnsFalseWhenDocumentNameDiffers() {
		RouteCriteria a = new RouteCriteria();
		a.setDocumentName("Contact");

		RouteCriteria b = new RouteCriteria();
		b.setDocumentName("User");

		assertFalse(a.matches(b));
	}

	@Test
	void matchesReturnsFalseWhenViewTypeDiffers() {
		RouteCriteria a = new RouteCriteria();
		a.setViewType(ViewType.edit);

		RouteCriteria b = new RouteCriteria();
		b.setViewType(ViewType.list);

		assertFalse(a.matches(b));
	}

	@Test
	void matchesReturnsFalseWhenWebActionDiffers() {
		RouteCriteria a = new RouteCriteria();
		a.setWebAction(WebAction.e);

		RouteCriteria b = new RouteCriteria();
		b.setWebAction(WebAction.l);

		assertFalse(a.matches(b));
	}

	@Test
	void matchesReturnsFalseWhenQueryNameDiffers() {
		RouteCriteria a = new RouteCriteria();
		a.setQueryName("qA");

		RouteCriteria b = new RouteCriteria();
		b.setQueryName("qB");

		assertFalse(a.matches(b));
	}

	@Test
	void matchesReturnsFalseWhenCustomerNameDiffers() {
		RouteCriteria a = new RouteCriteria();
		a.setCustomerName("demo");

		RouteCriteria b = new RouteCriteria();
		b.setCustomerName("other");

		assertFalse(a.matches(b));
	}

	@Test
	void matchesReturnsFalseWhenDataGroupIdDiffers() {
		RouteCriteria a = new RouteCriteria();
		a.setDataGroupId("dg1");

		RouteCriteria b = new RouteCriteria();
		b.setDataGroupId("dg2");

		assertFalse(a.matches(b));
	}

	@Test
	void matchesReturnsFalseWhenUserIdDiffers() {
		RouteCriteria a = new RouteCriteria();
		a.setUserId("user1");

		RouteCriteria b = new RouteCriteria();
		b.setUserId("user2");

		assertFalse(a.matches(b));
	}

	@Test
	void toStringContainsModuleName() {
		RouteCriteria rc = new RouteCriteria();
		rc.setModuleName("admin");
		assertTrue(rc.toString().contains("admin"));
	}

	@Test
	void toStringEmptyForNullFields() {
		RouteCriteria rc = new RouteCriteria();
		assertNotNull(rc.toString());
	}

	@Test
	void toStringContainsAllSetFields() {
		RouteCriteria rc = new RouteCriteria();
		rc.setModuleName("admin");
		rc.setDocumentName("Contact");
		rc.setCustomerName("demo");
		rc.setQueryName("qContacts");
		rc.setDataGroupId("dg1");
		rc.setUserId("user1");
		rc.setViewType(ViewType.edit);
		rc.setWebAction(WebAction.e);
		String s = rc.toString();
		assertTrue(s.contains("admin"));
		assertTrue(s.contains("Contact"));
		assertTrue(s.contains("demo"));
		assertTrue(s.contains("qContacts"));
	}

	@Test
	void matchesReturnsTrueWhenCustomerNameMatches() {
		RouteCriteria a = new RouteCriteria();
		a.setCustomerName("demo");

		RouteCriteria b = new RouteCriteria();
		b.setCustomerName("demo");

		assertTrue(a.matches(b));
	}

	@Test
	void matchesReturnsTrueWhenCriteriaFieldsAreNullButCandidateHasValues() {
		// empty 'a' matches anything
		RouteCriteria a = new RouteCriteria();

		RouteCriteria b = new RouteCriteria();
		b.setModuleName("admin");
		b.setDocumentName("Contact");
		b.setCustomerName("demo");

		assertTrue(a.matches(b));
	}

	@Test
	void matchesReturnsTrueWhenViewTypeMatches() {
		RouteCriteria a = new RouteCriteria();
		a.setViewType(ViewType.edit);
		RouteCriteria b = new RouteCriteria();
		b.setViewType(ViewType.edit);
		assertTrue(a.matches(b));
	}

	@Test
	void matchesReturnsTrueWhenWebActionMatches() {
		RouteCriteria a = new RouteCriteria();
		a.setWebAction(WebAction.e);
		RouteCriteria b = new RouteCriteria();
		b.setWebAction(WebAction.e);
		assertTrue(a.matches(b));
	}

	@Test
	void toStringContainsQueryNameDataGroupUserId() {
		RouteCriteria c = new RouteCriteria();
		c.setQueryName("myQuery");
		c.setDataGroupId("group1");
		c.setUserId("user1");
		String s = c.toString();
		assertTrue(s.contains("queryName=myQuery"));
		assertTrue(s.contains("dataGroupId=group1"));
		assertTrue(s.contains("userId=user1"));
	}

	@Test
	void toStringContainsViewTypeAndWebAction() {
		RouteCriteria c = new RouteCriteria();
		c.setViewType(ViewType.list);
		c.setWebAction(WebAction.l);
		String s = c.toString();
		assertTrue(s.contains("viewType=list"));
		assertTrue(s.contains("webAction=l"));
	}

	@Test
	void canonicaliseWithMapWebActionDoesNothing() {
		RouteCriteria rc = new RouteCriteria();
		rc.setWebAction(WebAction.m);
		rc.setModuleName("admin");
		rc.setDocumentName("Contact");
		// Should not throw and should not change module/document
		rc.canonicalise(null, "someBinding");
		assertEquals("admin", rc.getModuleName());
		assertEquals("Contact", rc.getDocumentName());
	}

	@Test
	void canonicaliseWithNullBindingDoesNothing() {
		RouteCriteria rc = new RouteCriteria();
		rc.setWebAction(WebAction.e);
		rc.setModuleName("admin");
		rc.setDocumentName("Contact");
		// null binding means processStringValue returns null -> skip
		rc.canonicalise(null, null);
		assertEquals("admin", rc.getModuleName());
		assertEquals("Contact", rc.getDocumentName());
	}

	@Test
	void canonicaliseWithBlankBindingDoesNothing() {
		RouteCriteria rc = new RouteCriteria();
		rc.setWebAction(WebAction.e);
		rc.setModuleName("admin");
		rc.setDocumentName("Contact");
		rc.canonicalise(null, "   ");
		assertEquals("admin", rc.getModuleName());
		assertEquals("Contact", rc.getDocumentName());
	}

	@Test
	void canonicaliseWithBindingAndNullModuleThrows() {
		RouteCriteria rc = new RouteCriteria();
		rc.setWebAction(WebAction.e);
		// moduleName is null
		assertThrows(IllegalStateException.class, () -> rc.canonicalise(null, "someBinding"));
	}

	@Test
	void canonicaliseWithBindingAndNullDocumentThrows() {
		RouteCriteria rc = new RouteCriteria();
		rc.setWebAction(WebAction.e);
		rc.setModuleName("admin");
		// documentName is null
		assertThrows(IllegalStateException.class, () -> rc.canonicalise(null, "someBinding"));
	}
}
