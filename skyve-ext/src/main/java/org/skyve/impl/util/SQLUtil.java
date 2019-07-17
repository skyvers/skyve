package org.skyve.impl.util;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.List;

import org.apache.commons.beanutils.DynaBean;
import org.skyve.domain.messages.DomainException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;

public class SQLUtil {
	private SQLUtil() {
		// no implementation
	}

	@SuppressWarnings("resource")
	public static List<DynaBean> retrieveListForSQL(Connection connection,
														String moduleName,
														String documentName,
														String sql,
														User user) {
		assert (sql != null);
		String newSQL = sql;
		
		SQLRowSetDynaClass rsdc = null;
		try {
			Customer customer = null;
			Module module = null;
			if ((user != null) && (moduleName != null)) {
				customer = user.getCustomer();
				module = customer.getModule(moduleName);
			}
			if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info(newSQL + " executed on thread " + Thread.currentThread() + ", connection = " + connection);
			Statement statement = connection.createStatement();
			ResultSet resultSet = statement.executeQuery(newSQL);

			try {
				rsdc = new SQLRowSetDynaClass(customer, module, documentName, resultSet);
			}
			finally {
				if (resultSet != null) {
					resultSet.close();
				}
				statement.close();
			}
		}
		catch (SQLException e) {
			throw new DomainException(e);
		}
		catch (Exception e) {
			throw new DomainException(e);
		}

		return rsdc.getRows();
	}
}
