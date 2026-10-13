package org.skyve.impl.persistence;

import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.inOrder;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;

import org.junit.jupiter.api.Test;
import org.mockito.InOrder;
import org.skyve.impl.persistence.hibernate.dialect.SkyveDialect;
import org.skyve.persistence.DataStore;

@SuppressWarnings({"static-method", "resource"})
class DynaIterableTest {
	@Test
	void testFetchSizeIsAppliedBeforeTheQueryExecutes() throws Exception {
		Connection connection = mock(Connection.class);
		PreparedStatement statement = mock(PreparedStatement.class);
		ResultSet resultSet = mock(ResultSet.class);
		when(connection.prepareStatement(anyString())).thenReturn(statement);
		when(statement.executeQuery()).thenReturn(resultSet);
		AbstractSQL sql = mock(AbstractSQL.class);
		when(sql.toQueryString()).thenReturn("select bizId from ADM_Contact");

		try (DynaIterable iterable = new DynaIterable(connection, sql, mock(DataStore.class), mock(SkyveDialect.class))) {
			InOrder inOrder = inOrder(statement);
			inOrder.verify(statement).setFetchSize(AbstractPersistence.JDBC_FETCH_SIZE);
			inOrder.verify(statement).executeQuery();
		}
	}
}
