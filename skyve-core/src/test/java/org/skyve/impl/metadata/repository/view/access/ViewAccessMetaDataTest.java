package org.skyve.impl.metadata.repository.view.access;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.when;

import java.util.Collections;
import java.util.TreeMap;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.user.UserAccess;

/**
 * Tests for small view access MetaData POJOs.
 */
@ExtendWith(MockitoExtension.class)
public class ViewAccessMetaDataTest {

	@Mock
	private Module mockModule;

	// ---- ViewReportUserAccessMetaData ----

	@Test
	@SuppressWarnings("static-method")
	void reportAccessSetModuleNameAndGet() {
		ViewReportUserAccessMetaData md = new ViewReportUserAccessMetaData();
		md.setModuleName("admin");
		assertThat(md.getModuleName(), is("admin"));
	}

	@Test
	@SuppressWarnings("static-method")
	void reportAccessSetDocumentNameAndGet() {
		ViewReportUserAccessMetaData md = new ViewReportUserAccessMetaData();
		md.setDocumentName("User");
		assertThat(md.getDocumentName(), is("User"));
	}

	@Test
	@SuppressWarnings("static-method")
	void reportAccessSetReportNameAndGet() {
		ViewReportUserAccessMetaData md = new ViewReportUserAccessMetaData();
		md.setReportName("UserReport");
		assertThat(md.getReportName(), is("UserReport"));
	}

	@Test
	@SuppressWarnings("static-method")
	void reportAccessBlankModuleNameBecomesNull() {
		ViewReportUserAccessMetaData md = new ViewReportUserAccessMetaData();
		md.setModuleName("  ");
		assertNull(md.getModuleName());
	}

	// ---- ViewQueryAggregateUserAccessMetaData ----

	@Test
	@SuppressWarnings("static-method")
	void queryAggregateAccessSetQueryNameAndGet() {
		ViewQueryAggregateUserAccessMetaData md = new ViewQueryAggregateUserAccessMetaData();
		md.setQueryName("qAllUsers");
		assertThat(md.getQueryName(), is("qAllUsers"));
	}

	@Test
	@SuppressWarnings("static-method")
	void queryAggregateAccessQueryNameBlankBecomesNull() {
		ViewQueryAggregateUserAccessMetaData md = new ViewQueryAggregateUserAccessMetaData();
		md.setQueryName("  ");
		assertNull(md.getQueryName());
	}

	// ---- ViewDocumentAggregateUserAccessMetaData ----

	@Test
	@SuppressWarnings("static-method")
	void documentAggregateAccessSetDocumentNameAndGet() {
		ViewDocumentAggregateUserAccessMetaData md = new ViewDocumentAggregateUserAccessMetaData();
		md.setDocumentName("Contact");
		assertThat(md.getDocumentName(), is("Contact"));
	}

	@Test
	@SuppressWarnings("static-method")
	void documentAggregateAccessDocumentNameBlankBecomesNull() {
		ViewDocumentAggregateUserAccessMetaData md = new ViewDocumentAggregateUserAccessMetaData();
		md.setDocumentName("  ");
		assertNull(md.getDocumentName());
	}

        @Test
        @SuppressWarnings("static-method")
        void reportAccessToUserAccessReturnsModelAggregate() {
                ViewReportUserAccessMetaData md = new ViewReportUserAccessMetaData();
                md.setModuleName("admin");
                md.setDocumentName("User");
                md.setReportName("UserReport");
                UserAccess ua = md.toUserAccess("admin", "User");
                assertEquals("admin", ua.getModuleName());
                assertEquals("User", ua.getDocumentName());
                assertEquals("UserReport", ua.getComponent());
        }

        @Test
        @SuppressWarnings("static-method")
        void queryAggregateAccessToUserAccessReturnsQueryAggregate() {
                ViewQueryAggregateUserAccessMetaData md = new ViewQueryAggregateUserAccessMetaData();
                md.setQueryName("qAllUsers");
                UserAccess ua = md.toUserAccess("admin", null);
                assertEquals("admin", ua.getModuleName());
                assertEquals("qAllUsers", ua.getComponent());
        }

        @Test
        @SuppressWarnings("static-method")
        void documentAggregateAccessToUserAccessReturnsDocumentAggregate() {
                ViewDocumentAggregateUserAccessMetaData md = new ViewDocumentAggregateUserAccessMetaData();
                md.setDocumentName("Contact");
                UserAccess ua = md.toUserAccess("admin", null);
                assertEquals("admin", ua.getModuleName());
                assertEquals("Contact", ua.getComponent());
        }

        @Test
        @SuppressWarnings("static-method")
        void reportAccessValidateThrowsWhenModuleNameNull() {
                ViewReportUserAccessMetaData md = new ViewReportUserAccessMetaData();
                assertThrows(MetaDataException.class, () -> md.validate("test", null));
        }

        @Test
        @SuppressWarnings("static-method")
        void reportAccessValidateThrowsWhenDocumentNameNull() {
                ViewReportUserAccessMetaData md = new ViewReportUserAccessMetaData();
                md.setModuleName("admin");
                assertThrows(MetaDataException.class, () -> md.validate("test", null));
        }

        @Test
        @SuppressWarnings("static-method")
        void reportAccessValidateThrowsWhenReportNameNull() {
                ViewReportUserAccessMetaData md = new ViewReportUserAccessMetaData();
                md.setModuleName("admin");
                md.setDocumentName("User");
                assertThrows(MetaDataException.class, () -> md.validate("test", null));
        }

        @Test
        @SuppressWarnings("static-method")
        void queryAggregateAccessValidateThrowsWhenQueryNameNull() {
                ViewQueryAggregateUserAccessMetaData md = new ViewQueryAggregateUserAccessMetaData();
                assertThrows(MetaDataException.class, () -> md.validate("test", null));
        }

        @Test
        @SuppressWarnings("static-method")
        void documentAggregateAccessValidateThrowsWhenDocumentNameNull() {
                ViewDocumentAggregateUserAccessMetaData md = new ViewDocumentAggregateUserAccessMetaData();
                assertThrows(MetaDataException.class, () -> md.validate("test", null));
        }

        @Test
        @SuppressWarnings("static-method")
        void contentAccessSetBindingAndGet() {
                ViewContentUserAccessMetaData md = new ViewContentUserAccessMetaData();
                md.setBinding("attachment");
                assertEquals("attachment", md.getBinding());
        }

        @Test
        @SuppressWarnings("static-method")
        void contentAccessToUserAccessReturnsContent() {
                ViewContentUserAccessMetaData md = new ViewContentUserAccessMetaData();
                md.setBinding("attachment");
                UserAccess ua = md.toUserAccess("admin", "Contact");
                assertEquals("admin", ua.getModuleName());
                assertEquals("Contact", ua.getDocumentName());
                assertEquals("attachment", ua.getComponent());
        }

        @Test
        @SuppressWarnings("static-method")
        void contentAccessValidateThrowsWhenBindingNull() {
                ViewContentUserAccessMetaData md = new ViewContentUserAccessMetaData();
                assertThrows(MetaDataException.class, () -> md.validate("test", null));
        }

        @Test
        @SuppressWarnings("static-method")
        void dynamicImageAccessValidateThrowsWhenImageNameNull() {
                ViewDynamicImageUserAccessMetaData md = new ViewDynamicImageUserAccessMetaData();
                assertThrows(MetaDataException.class, () -> md.validate("test", null));
        }

        @Test
        @SuppressWarnings("static-method")
        void modelAggregateAccessValidateThrowsWhenModelNameNull() {
                ViewModelAggregateUserAccessMetaData md = new ViewModelAggregateUserAccessMetaData();
                assertThrows(MetaDataException.class, () -> md.validate("test", null));
        }

        @Test
        @SuppressWarnings("static-method")
        void previousCompleteAccessValidateThrowsWhenBindingNull() {
                ViewPreviousCompleteUserAccessMetaData md = new ViewPreviousCompleteUserAccessMetaData();
                assertThrows(MetaDataException.class, () -> md.validate("test", null));
        }

        // ---- Validate happy-path tests ----

        @Test
        @SuppressWarnings("static-method")
        void contentAccessValidatePassesWhenBindingSet() {
                ViewContentUserAccessMetaData md = new ViewContentUserAccessMetaData();
                md.setBinding("attachment");
                assertDoesNotThrow(() -> md.validate("test", null));
        }

        @Test
        @SuppressWarnings("static-method")
        void dynamicImageAccessValidatePassesWhenImageNameSet() {
                ViewDynamicImageUserAccessMetaData md = new ViewDynamicImageUserAccessMetaData();
                md.setImageName("profilePhoto");
                assertDoesNotThrow(() -> md.validate("test", null));
        }

        @Test
        @SuppressWarnings("static-method")
        void modelAggregateAccessValidatePassesWhenModelNameSet() {
                ViewModelAggregateUserAccessMetaData md = new ViewModelAggregateUserAccessMetaData();
                md.setModelName("ContactList");
                assertDoesNotThrow(() -> md.validate("test", null));
        }

        @Test
        @SuppressWarnings("static-method")
        void previousCompleteAccessValidatePassesWhenBindingSet() {
                ViewPreviousCompleteUserAccessMetaData md = new ViewPreviousCompleteUserAccessMetaData();
                md.setBinding("approvalStep");
                assertDoesNotThrow(() -> md.validate("test", null));
        }

        @Test
        @SuppressWarnings("static-method")
        void reportAccessValidatePassesWhenAllFieldsSet() {
                ViewReportUserAccessMetaData md = new ViewReportUserAccessMetaData();
                md.setModuleName("admin");
                md.setDocumentName("User");
                md.setReportName("UserReport");
                assertDoesNotThrow(() -> md.validate("test", null));
        }

        // ---- Module-dependent validate tests ----

        @Test
        void documentAggregateAccessValidatePassesWhenDocumentInModule() {
                ViewDocumentAggregateUserAccessMetaData md = new ViewDocumentAggregateUserAccessMetaData();
                md.setDocumentName("Contact");
                TreeMap<String, Module.DocumentRef> docRefs = new TreeMap<>();
                docRefs.put("Contact", new Module.DocumentRef());
                when(mockModule.getDocumentRefs()).thenReturn(Collections.unmodifiableMap(docRefs));
                assertDoesNotThrow(() -> md.validate("test", mockModule));
        }

        @Test
        void documentAggregateAccessValidateThrowsWhenDocumentNotInModule() {
                ViewDocumentAggregateUserAccessMetaData md = new ViewDocumentAggregateUserAccessMetaData();
                md.setDocumentName("UnknownDoc");
                when(mockModule.getDocumentRefs()).thenReturn(Collections.emptyMap());
                when(mockModule.getName()).thenReturn("admin");
                assertThrows(MetaDataException.class, () -> md.validate("test", mockModule));
        }

        @Test
        void queryAggregateAccessValidatePassesWhenQueryInModule() {
                ViewQueryAggregateUserAccessMetaData md = new ViewQueryAggregateUserAccessMetaData();
                md.setQueryName("qAllContacts");
                when(mockModule.getMetaDataQuery("qAllContacts")).thenReturn(org.mockito.Mockito.mock(MetaDataQueryDefinition.class));
                assertDoesNotThrow(() -> md.validate("test", mockModule));
        }

        @Test
        void queryAggregateAccessValidateThrowsWhenQueryNotInModule() {
                ViewQueryAggregateUserAccessMetaData md = new ViewQueryAggregateUserAccessMetaData();
                md.setQueryName("missingQuery");
                when(mockModule.getMetaDataQuery("missingQuery")).thenReturn(null);
                when(mockModule.getName()).thenReturn("admin");
                assertThrows(MetaDataException.class, () -> md.validate("test", mockModule));
        }
}
