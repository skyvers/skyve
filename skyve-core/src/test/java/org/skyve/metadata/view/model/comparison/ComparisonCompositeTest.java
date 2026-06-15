package org.skyve.metadata.view.model.comparison;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.view.model.comparison.ComparisonComposite.Mutation;

class ComparisonCompositeTest {

	@Test
	@SuppressWarnings("static-method")
	void defaultConstructorInitialisesCollections() {
		ComparisonComposite cc = new ComparisonComposite();
		assertNotNull(cc.getProperties());
		assertNotNull(cc.getChildren());
		assertEquals(0, cc.getProperties().size());
		assertEquals(0, cc.getChildren().size());
	}

	@Test
	@SuppressWarnings("static-method")
	void sixArgConstructorSetsFields() {
		ComparisonComposite cc = new ComparisonComposite("id1", "bizKey", "ref", "relation", Mutation.added, null);
		assertThat(cc.getBizId(), is("id1"));
		assertThat(cc.getBusinessKeyDescription(), is("bizKey"));
		assertThat(cc.getReferenceName(), is("ref"));
		assertThat(cc.getRelationshipDescription(), is("relation"));
		assertThat(cc.getMutation(), is(Mutation.added));
		assertNull(cc.getDocument());
	}

	@Test
	@SuppressWarnings("static-method")
	void setBizIdRoundtrip() {
		ComparisonComposite cc = new ComparisonComposite();
		cc.setBizId("abc");
		assertThat(cc.getBizId(), is("abc"));
	}

	@Test
	@SuppressWarnings("static-method")
	void setBusinessKeyDescriptionRoundtrip() {
		ComparisonComposite cc = new ComparisonComposite();
		cc.setBusinessKeyDescription("myKey");
		assertThat(cc.getBusinessKeyDescription(), is("myKey"));
	}

	@Test
	@SuppressWarnings("static-method")
	void setReferenceNameRoundtrip() {
		ComparisonComposite cc = new ComparisonComposite();
		cc.setReferenceName("ref");
		assertThat(cc.getReferenceName(), is("ref"));
	}

	@Test
	@SuppressWarnings("static-method")
	void setRelationshipDescriptionRoundtrip() {
		ComparisonComposite cc = new ComparisonComposite();
		cc.setRelationshipDescription("desc");
		assertThat(cc.getRelationshipDescription(), is("desc"));
	}

	@Test
	@SuppressWarnings("static-method")
	void setMutationRoundtrip() {
		ComparisonComposite cc = new ComparisonComposite();
		cc.setMutation(Mutation.deleted);
		assertThat(cc.getMutation(), is(Mutation.deleted));
	}

	@Test
	@SuppressWarnings("static-method")
	void determineMutationsUnchangedWhenNoDirtyProperties() {
		ComparisonComposite cc = new ComparisonComposite();
		cc.setMutation(Mutation.unchanged);
		ComparisonProperty p = new ComparisonProperty();
		p.setOldValue("val");
		p.setNewValue("val");
		cc.getProperties().add(p);
		cc.determineMutations();
		assertThat(cc.getMutation(), is(Mutation.unchanged));
	}

	@Test
	@SuppressWarnings("static-method")
	void determineMutationsUpdatedWhenDirtyProperty() {
		ComparisonComposite cc = new ComparisonComposite();
		cc.setMutation(Mutation.unchanged);
		ComparisonProperty p = new ComparisonProperty();
		p.setOldValue("oldVal");
		p.setNewValue("newVal");
		cc.getProperties().add(p);
		cc.determineMutations();
		assertThat(cc.getMutation(), is(Mutation.updated));
	}

	@Test
	@SuppressWarnings("static-method")
	void determineMutationsAddedPreserved() {
		ComparisonComposite cc = new ComparisonComposite();
		cc.setMutation(Mutation.added);
		cc.determineMutations();
		assertThat(cc.getMutation(), is(Mutation.added));
	}

	@Test
	@SuppressWarnings("static-method")
	void determineMutationsNullMutationUpdatedWhenDirtyProperty() {
		ComparisonComposite cc = new ComparisonComposite();
		// mutation starts null
		ComparisonProperty p = new ComparisonProperty();
		p.setOldValue("oldVal");
		p.setNewValue("newVal");
		cc.getProperties().add(p);
		cc.determineMutations();
		assertThat(cc.getMutation(), is(Mutation.updated));
	}

	@Test
	@SuppressWarnings("static-method")
	void childDirtyPropagatesAndSetsChildMutation() {
		ComparisonComposite parent = new ComparisonComposite();
		parent.setMutation(Mutation.unchanged);

		ComparisonComposite child = new ComparisonComposite();
		child.setMutation(Mutation.unchanged);
		ComparisonProperty p = new ComparisonProperty();
		p.setOldValue("oldVal");
		p.setNewValue("newVal");
		child.getProperties().add(p);
		parent.getChildren().add(child);

		parent.determineMutations();

		assertThat(child.getMutation(), is(Mutation.updated));
		assertThat(parent.getMutation(), is(Mutation.updated));
	}

	@Test
	@SuppressWarnings("static-method")
	void mutationEnumValues() {
		Mutation[] values = Mutation.values();
		assertEquals(4, values.length);
		assertThat(Mutation.valueOf("unchanged"), is(Mutation.unchanged));
		assertThat(Mutation.valueOf("added"), is(Mutation.added));
		assertThat(Mutation.valueOf("updated"), is(Mutation.updated));
		assertThat(Mutation.valueOf("deleted"), is(Mutation.deleted));
	}

	@Test
	@SuppressWarnings("static-method")
	void determineMutationsDeletedPreserved() {
		ComparisonComposite cc = new ComparisonComposite();
		cc.setMutation(Mutation.deleted);
		cc.determineMutations();
		assertThat(cc.getMutation(), is(Mutation.deleted));
	}

	@Test
	@SuppressWarnings("static-method")
	void setDocumentRoundtrip() {
		ComparisonComposite cc = new ComparisonComposite();
		org.skyve.metadata.model.document.Document doc = org.mockito.Mockito.mock(org.skyve.metadata.model.document.Document.class);
		cc.setDocument(doc);
		assertThat(cc.getDocument(), is(doc));
	}

	@Test
	@SuppressWarnings("static-method")
	void determineMutationsNullMutationUnchangedWhenNoDirtyPropertiesOrChildren() {
		ComparisonComposite cc = new ComparisonComposite();
		// mutation starts null, no properties, no children
		cc.determineMutations();
		assertNull(cc.getMutation());
	}
}
