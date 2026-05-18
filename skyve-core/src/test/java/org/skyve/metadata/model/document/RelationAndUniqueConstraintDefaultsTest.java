package org.skyve.metadata.model.document;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertSame;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.model.document.AssociationImpl;
import org.skyve.impl.metadata.model.document.UniqueConstraintImpl;
import org.skyve.metadata.user.DocumentPermissionScope;

@SuppressWarnings("static-method")
class RelationAndUniqueConstraintDefaultsTest {
	@Test
	void relationDefaultIsScalarReturnsFalse() {
		Relation relation = new AssociationImpl();
		assertFalse(relation.isScalar());
	}

	@Test
	void uniqueConstraintDocumentScopeMapsToPermissionScope() {
		assertSame(DocumentPermissionScope.global, UniqueConstraint.DocumentScope.global.toDocumentPermissionScope());
		assertSame(DocumentPermissionScope.customer, UniqueConstraint.DocumentScope.customer.toDocumentPermissionScope());
		assertSame(DocumentPermissionScope.dataGroup, UniqueConstraint.DocumentScope.dataGroup.toDocumentPermissionScope());
		assertSame(DocumentPermissionScope.user, UniqueConstraint.DocumentScope.user.toDocumentPermissionScope());
	}

	@Test
	void uniqueConstraintDefaultLocalisedDescriptionReturnsDescription() {
		UniqueConstraintImpl constraint = new UniqueConstraintImpl();
		constraint.setDescription("Duplicate value");
		assertSame(constraint.getDescription(), constraint.getLocalisedDescription());
	}
}
