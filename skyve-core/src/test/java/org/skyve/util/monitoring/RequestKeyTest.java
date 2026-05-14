package org.skyve.util.monitoring;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.model.document.Bizlet.DomainValue;

public class RequestKeyTest {

	// ---- RequestKey.NONE ----

	@Test
	@SuppressWarnings("static-method")
	public void noneToStringReturnsNull() {
		assertNull(RequestKey.NONE.toString());
	}

	@Test
	@SuppressWarnings("static-method")
	public void noneTypeIsSpace() {
		assertEquals(' ', RequestKey.NONE.getType());
	}

	// ---- documentListModel factory ----

	@Test
	@SuppressWarnings("static-method")
	public void documentListModelToString() {
		RequestKey key = RequestKey.documentListModel("admin", "User");
		assertThat(key.toString(), is("Madmin.User"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void documentListModelGetType() {
		RequestKey key = RequestKey.documentListModel("admin", "User");
		assertEquals('M', key.getType());
	}

	@Test
	@SuppressWarnings("static-method")
	public void documentListModelGetModuleName() {
		RequestKey key = RequestKey.documentListModel("admin", "User");
		assertThat(key.getModuleName(), is("admin"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void documentListModelGetDocumentName() {
		RequestKey key = RequestKey.documentListModel("admin", "User");
		assertThat(key.getDocumentName(), is("User"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void documentListModelGetComponentNull() {
		RequestKey key = RequestKey.documentListModel("admin", "User");
		assertNull(key.getComponent());
	}

	// ---- queryListModel factory ----

	@Test
	@SuppressWarnings("static-method")
	public void queryListModelToString() {
		// type='M', module="admin", doc=null, component="qAllContacts"
		// toString → "Madmin^qAllContacts"
		RequestKey key = RequestKey.queryListModel("admin", "qAllContacts");
		assertThat(key.toString(), is("Madmin^qAllContacts"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void queryListModelGetModuleName() {
		RequestKey key = RequestKey.queryListModel("admin", "qAllContacts");
		assertThat(key.getModuleName(), is("admin"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void queryListModelGetDocumentNameNull() {
		RequestKey key = RequestKey.queryListModel("admin", "qAllContacts");
		assertNull(key.getDocumentName());
	}

	@Test
	@SuppressWarnings("static-method")
	public void queryListModelGetComponent() {
		RequestKey key = RequestKey.queryListModel("admin", "qAllContacts");
		assertThat(key.getComponent(), is("qAllContacts"));
	}

	// ---- fromString round-trips ----

	@Test
	@SuppressWarnings("static-method")
	public void fromStringWithModuleAndDocument() {
		// format: {type}{module}.{document}
		RequestKey key = RequestKey.fromString("Eadmin.User");
		assertEquals('E', key.getType());
		assertThat(key.getModuleName(), is("admin"));
		assertThat(key.getDocumentName(), is("User"));
		assertNull(key.getComponent());
	}

	@Test
	@SuppressWarnings("static-method")
	public void fromStringWithModuleDocumentAndComponent() {
		// format: {type}{module}.{document}^{component}
		RequestKey key = RequestKey.fromString("Oadmin.Contact^firstName");
		assertEquals('O', key.getType());
		assertThat(key.getModuleName(), is("admin"));
		assertThat(key.getDocumentName(), is("Contact"));
		assertThat(key.getComponent(), is("firstName"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void fromStringWithModuleAndComponent() {
		// format: {type}{module}^{component}
		RequestKey key = RequestKey.fromString("Madmin^qAllContacts");
		assertEquals('M', key.getType());
		assertThat(key.getModuleName(), is("admin"));
		assertNull(key.getDocumentName());
		assertThat(key.getComponent(), is("qAllContacts"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void fromStringWithModuleOnly() {
		// format: {type}{module}
		RequestKey key = RequestKey.fromString("Cadmin");
		assertEquals('C', key.getType());
		assertThat(key.getModuleName(), is("admin"));
		assertNull(key.getDocumentName());
		assertNull(key.getComponent());
	}

	@Test
	@SuppressWarnings("static-method")
	public void documentListModelRoundTripViaFromString() {
		RequestKey original = RequestKey.documentListModel("admin", "User");
		String encoded = original.toString();
		RequestKey parsed = RequestKey.fromString(encoded);
		assertEquals(original.getType(), parsed.getType());
		assertThat(parsed.getModuleName(), is(original.getModuleName()));
		assertThat(parsed.getDocumentName(), is(original.getDocumentName()));
	}

	@Test
	@SuppressWarnings("static-method")
	public void queryListModelRoundTripViaFromString() {
		RequestKey original = RequestKey.queryListModel("admin", "qAllContacts");
		String encoded = original.toString();
		RequestKey parsed = RequestKey.fromString(encoded);
		assertEquals(original.getType(), parsed.getType());
		assertThat(parsed.getModuleName(), is(original.getModuleName()));
		assertThat(parsed.getComponent(), is(original.getComponent()));
	}

	// ---- toDomainValue ----

	@Test
	@SuppressWarnings("static-method")
	public void toDomainValueCreatesNonNullDomainValue() {
		RequestKey key = RequestKey.documentListModel("admin", "User");
		DomainValue dv = key.toDomainValue();
		assertNotNull(dv);
	}

	@Test
	@SuppressWarnings("static-method")
	public void toDomainValueCodeMatchesToString() {
		RequestKey key = RequestKey.documentListModel("admin", "User");
		DomainValue dv = key.toDomainValue();
		assertEquals(key.toString(), dv.getCode());
	}

	// ---- toDescription ----

	@Test
	@SuppressWarnings("static-method")
	public void toDescriptionModelKey() {
		RequestKey key = RequestKey.documentListModel("admin", "User");
		assertThat(key.toDescription(), is("Model admin.User"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void toDescriptionQueryListModelKey() {
		// queryListModel: type='M', module="admin", doc=null, component="qAllContacts"
		RequestKey key = RequestKey.queryListModel("admin", "qAllContacts");
		assertThat(key.toDescription(), is("Model admin qAllContacts"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void toDescriptionFromStringCreateType() {
		RequestKey key = RequestKey.fromString("Cadmin.User");
		assertThat(key.toDescription(), is("Create admin.User"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void toDescriptionFromStringEditType() {
		RequestKey key = RequestKey.fromString("Eadmin.User");
		assertThat(key.toDescription(), is("Edit admin.User"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void toDescriptionFromStringSaveType() {
		RequestKey key = RequestKey.fromString("Sadmin.User");
		assertThat(key.toDescription(), is("Save admin.User"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void toDescriptionFromStringDeleteType() {
		RequestKey key = RequestKey.fromString("Dadmin.User");
		assertThat(key.toDescription(), is("Delete admin.User"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void toDescriptionFromStringZoomOutType() {
		RequestKey key = RequestKey.fromString("Zadmin.User");
		assertThat(key.toDescription(), is("Zoom Out admin.User"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void toDescriptionFromStringRerenderType() {
		RequestKey key = RequestKey.fromString("Radmin.User");
		assertThat(key.toDescription(), is("Rerender admin.User"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void toDescriptionFromStringActionType() {
		RequestKey key = RequestKey.fromString("Aadmin.User^myAction");
		assertThat(key.toDescription(), is("Action admin.User myAction"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void toDescriptionFromStringCompleteType() {
		RequestKey key = RequestKey.fromString("Oadmin.Contact^firstName");
		assertThat(key.toDescription(), is("Complete admin.Contact firstName"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void toDescriptionFromStringDynamicImageType() {
		RequestKey key = RequestKey.fromString("Iadmin.User^myImage");
		assertThat(key.toDescription(), is("Dynamic Image admin.User myImage"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void toDescriptionFromStringUnknownType() {
		RequestKey key = RequestKey.fromString("Xadmin.User");
		assertThat(key.toDescription(), is("Unknown admin.User"));
	}

	// ---- fromString edge cases ----

	@Test
	@SuppressWarnings("static-method")
	public void fromStringWithNoModuleOnlyType() {
		// remaining is empty → module=null
		RequestKey key = RequestKey.fromString("C");
		assertEquals('C', key.getType());
		assertNull(key.getModuleName());
		assertNull(key.getDocumentName());
		assertNull(key.getComponent());
	}
}
