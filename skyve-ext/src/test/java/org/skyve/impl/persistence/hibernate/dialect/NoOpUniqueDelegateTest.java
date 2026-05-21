package org.skyve.impl.persistence.hibernate.dialect;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.mockito.Mockito.mock;

import org.hibernate.boot.Metadata;
import org.hibernate.mapping.UniqueKey;
import org.junit.Test;

@SuppressWarnings("static-method")
public class NoOpUniqueDelegateTest {

	@Test
	public void getAlterTableToAddUniqueKeyCommandReturnsEmpty() {
		H2SpatialDialect dialect = new H2SpatialDialect();
		NoOpUniqueDelegate delegate = new NoOpUniqueDelegate(dialect);
		UniqueKey uk = mock(UniqueKey.class);
		Metadata metadata = mock(Metadata.class);
		assertEquals("", delegate.getAlterTableToAddUniqueKeyCommand(uk, metadata));
	}

	@Test
	public void getAlterTableToDropUniqueKeyCommandReturnsEmpty() {
		H2SpatialDialect dialect = new H2SpatialDialect();
		NoOpUniqueDelegate delegate = new NoOpUniqueDelegate(dialect);
		UniqueKey uk = mock(UniqueKey.class);
		Metadata metadata = mock(Metadata.class);
		assertEquals("", delegate.getAlterTableToDropUniqueKeyCommand(uk, metadata));
	}

	@Test
	public void constructorWithDialectDoesNotThrow() {
		H2SpatialDialect dialect = new H2SpatialDialect();
		NoOpUniqueDelegate delegate = new NoOpUniqueDelegate(dialect);
		assertNotNull(delegate);
	}
}
