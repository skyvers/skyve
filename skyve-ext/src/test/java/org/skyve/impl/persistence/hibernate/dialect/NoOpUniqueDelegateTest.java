package org.skyve.impl.persistence.hibernate.dialect;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.Mockito.mock;

import org.hibernate.boot.Metadata;
import org.hibernate.mapping.UniqueKey;
import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class NoOpUniqueDelegateTest {

	@Test
	void getAlterTableToAddUniqueKeyCommandReturnsEmpty() {
		H2SpatialDialect dialect = new H2SpatialDialect();
		NoOpUniqueDelegate delegate = new NoOpUniqueDelegate(dialect);
		UniqueKey uk = mock(UniqueKey.class);
		Metadata metadata = mock(Metadata.class);
		assertEquals("", delegate.getAlterTableToAddUniqueKeyCommand(uk, metadata));
	}

	@Test
	void getAlterTableToDropUniqueKeyCommandReturnsEmpty() {
		H2SpatialDialect dialect = new H2SpatialDialect();
		NoOpUniqueDelegate delegate = new NoOpUniqueDelegate(dialect);
		UniqueKey uk = mock(UniqueKey.class);
		Metadata metadata = mock(Metadata.class);
		assertEquals("", delegate.getAlterTableToDropUniqueKeyCommand(uk, metadata));
	}

	@Test
	void constructorWithDialectDoesNotThrow() {
		H2SpatialDialect dialect = new H2SpatialDialect();
		NoOpUniqueDelegate delegate = new NoOpUniqueDelegate(dialect);
		assertNotNull(delegate);
	}
}
