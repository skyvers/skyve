package org.skyve.impl.metadata.customer;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.model.document.Association.AssociationType;
import org.skyve.metadata.model.document.Collection.CollectionType;
import org.skyve.metadata.model.document.Reference.ReferenceType;

class ExportedReferenceTest {

	@Test
	@SuppressWarnings("static-method")
	void setModuleNameRoundtrip() {
		ExportedReference ref = new ExportedReference();
		ref.setModuleName("admin");
		assertThat(ref.getModuleName(), is("admin"));
	}

	@Test
	@SuppressWarnings("static-method")
	void setDocumentNameRoundtrip() {
		ExportedReference ref = new ExportedReference();
		ref.setDocumentName("User");
		assertThat(ref.getDocumentName(), is("User"));
	}

	@Test
	@SuppressWarnings("static-method")
	void setDocumentAliasRoundtrip() {
		ExportedReference ref = new ExportedReference();
		ref.setDocumentAlias("admin.User.documentAlias");
		assertThat(ref.getDocumentAlias(), is("admin.User.documentAlias"));
	}

	@Test
	@SuppressWarnings("static-method")
	void setReferenceFieldNameRoundtrip() {
		ExportedReference ref = new ExportedReference();
		ref.setReferenceFieldName("contact");
		assertThat(ref.getReferenceFieldName(), is("contact"));
	}

	@Test
	@SuppressWarnings("static-method")
	void setRequiredRoundtrip() {
		ExportedReference ref = new ExportedReference();
		assertFalse(ref.isRequired());
		ref.setRequired(true);
		assertTrue(ref.isRequired());
	}

	@Test
	@SuppressWarnings("static-method")
	void setTypeToReferenceTypeIsNotCollection() {
		ExportedReference ref = new ExportedReference();
		ref.setType(AssociationType.aggregation);
		assertFalse(ref.isCollection());
		assertThat(ref.getType(), is((ReferenceType) AssociationType.aggregation));
	}

	@Test
	@SuppressWarnings("static-method")
	void setTypeToCollectionTypeIsCollection() {
		ExportedReference ref = new ExportedReference();
		ref.setType(CollectionType.child);
		assertTrue(ref.isCollection());
	}

        @Test
        @SuppressWarnings("static-method")
        void getLocalisedDocumentAliasWithNullReturnsNull() {
                ExportedReference ref = new ExportedReference();
                assertThat(ref.getLocalisedDocumentAlias(), is(org.hamcrest.Matchers.nullValue()));
        }

        @Test
        @SuppressWarnings("static-method")
        void setPersistentAndGetRoundtrip() {
                ExportedReference ref = new ExportedReference();
                assertThat(ref.getPersistent(), is(org.hamcrest.Matchers.nullValue()));
                ref.setPersistent(null);
                assertThat(ref.getPersistent(), is(org.hamcrest.Matchers.nullValue()));
        }
}
