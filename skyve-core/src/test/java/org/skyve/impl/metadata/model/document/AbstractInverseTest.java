package org.skyve.impl.metadata.model.document;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.model.document.AbstractInverse.InverseRelationship;
import org.skyve.metadata.model.document.Inverse.InverseCardinality;

/**
 * Tests for AbstractInverse (via InverseMany) and InverseMany itself.
 */
public class AbstractInverseTest {

	@Test
	@SuppressWarnings("static-method")
	void setDocumentNameAndGet() {
		InverseMany inv = new InverseMany();
		inv.setDocumentName("Contact");
		assertThat(inv.getDocumentName(), is("Contact"));
	}

	@Test
	@SuppressWarnings("static-method")
	void documentNameBlankBecomesNull() {
		InverseMany inv = new InverseMany();
		inv.setDocumentName("  ");
		assertNull(inv.getDocumentName());
	}

	@Test
	@SuppressWarnings("static-method")
	void setReferenceNameAndGet() {
		InverseMany inv = new InverseMany();
		inv.setReferenceName("contacts");
		assertThat(inv.getReferenceName(), is("contacts"));
	}

	@Test
	@SuppressWarnings("static-method")
	void setRelationshipAndGet() {
		InverseMany inv = new InverseMany();
		inv.setRelationship(InverseRelationship.oneToMany);
		assertThat(inv.getRelationship(), is(InverseRelationship.oneToMany));
	}

	@Test
	@SuppressWarnings("static-method")
	void setCascadeAndGet() {
		InverseMany inv = new InverseMany();
		inv.setCascade(Boolean.TRUE);
		assertThat(inv.getCascade(), is(Boolean.TRUE));
	}

	@Test
	@SuppressWarnings("static-method")
	void isRequiredReturnsFalse() {
		InverseMany inv = new InverseMany();
		assertFalse(inv.isRequired());
	}

	@Test
	@SuppressWarnings("static-method")
	void isPersistentReturnsTrue() {
		InverseMany inv = new InverseMany();
		assertTrue(inv.isPersistent());
	}

	// InverseMany-specific

	@Test
	@SuppressWarnings("static-method")
	void inverseManyCardinalityIsMany() {
		InverseMany inv = new InverseMany();
		assertThat(inv.getCardinality(), is(InverseCardinality.many));
	}

	@Test
	@SuppressWarnings("static-method")
	void inverseManyOrderingListNotNull() {
		InverseMany inv = new InverseMany();
		assertNotNull(inv.getOrdering());
	}

	@Test
	@SuppressWarnings("static-method")
	void inverseManyComplexOrderingDefaultsFalse() {
		InverseMany inv = new InverseMany();
		assertFalse(inv.isComplexOrdering());
	}

	@Test
	@SuppressWarnings("static-method")
	void inverseManySetComplexOrderingAndGet() {
		InverseMany inv = new InverseMany();
		inv.setComplexOrdering(true);
		assertTrue(inv.isComplexOrdering());
	}

	@Test
	@SuppressWarnings("static-method")
	void setDomainTypeAndGet() {
		InverseMany inv = new InverseMany();
		inv.setDomainType(org.skyve.metadata.model.document.DomainType.constant);
		assertThat(inv.getDomainType(), is(org.skyve.metadata.model.document.DomainType.constant));
	}
}
