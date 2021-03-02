package services.report;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.util.ArrayList;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import javax.inject.Singleton;

import org.apache.commons.beanutils.DynaBean;
import org.apache.commons.beanutils.LazyDynaMap;
import org.apache.commons.beanutils.PropertyUtils;
import org.apache.commons.beanutils.ResultSetDynaClass;
import org.skyve.EXT;
import org.skyve.domain.types.DateTime;
import org.skyve.impl.dataaccess.sql.NamedParameterPreparedStatement;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import modules.admin.domain.ReportDataset;

/**
 * Utility service class to retrieve data from the data store for use in a {@link ReportDataset}.
 */
@Singleton
public class SqlQueryService {

	private static final Logger LOG = LoggerFactory.getLogger(SqlQueryService.class);

	/**
	 * Retrieves a list of BeanUtils {@link DynaBean} from the current application database
	 * connection. Columns are returned as properties of the dyna bean matching the case 
	 * specified in the query.
	 * 
	 * @param query String representing the sql query to be executed against the datastore
	 * @param limit The optional number of results to return if performing a sample query
	 * @return List of dynabeans containing the results of the query, or null if no results
	 * @throws Exception
	 */
	public List<DynaBean> retrieveDynamicResults(final String query, final Integer... limit) throws Exception {
		if (query != null) {
			try (Connection connection = EXT.getDataStoreConnection()) {
				final String sql = limitQuery(query, limit);

				final PreparedStatement statement = connection.prepareStatement(sql);

				try (ResultSet rs = statement.executeQuery()) {
					return parseResultSet(rs);
				}
			}
		}

		LOG.warn("Query string not supplied, no results returned.");
		return null;
	}

	/**
	 * Retrieves a list of BeanUtils {@link DynaBean} from the current application database
	 * connection. Columns are returned as properties of the dyna bean matching the case specified
	 * in the query.
	 * 
	 * @param query String representing the sql query to be executed against the datastore
	 * @param parameters A Map of <parameter name, value> to set in the query
	 * @return List of dynabeans containing the results of the query, or null if no results
	 * @throws Exception
	 */
	public List<DynaBean> retrieveDynamicResults(final String query, final Map<String, Object> parameters) throws Exception {
		if (query != null) {
			try (Connection connection = EXT.getDataStoreConnection()) {
				try (NamedParameterPreparedStatement statement = new NamedParameterPreparedStatement(connection, query)) {
					LOG.debug("Executing sql query: {}", query);
					// inject the parameters into the query
					if (parameters != null) {
						for (final Map.Entry<String, Object> e : parameters.entrySet()) {
							if (e.getValue() instanceof Date) {
								Date dt = (Date) e.getValue();
								statement.setDate(e.getKey(), new java.sql.Date(dt.getTime()));
							} else if (e.getValue() instanceof DateTime) {
								DateTime dt = (DateTime) e.getValue();
								statement.setDate(e.getKey(), new java.sql.Date(dt.getTime()));
							} else if (e.getValue() instanceof Integer) {
								statement.setInt(e.getKey(), (Integer) e.getValue());
							} else {
								statement.setString(e.getKey(), (String) e.getValue());
							}
						}
					}

					try (ResultSet rs = statement.executeQuery()) {
						return parseResultSet(rs);
					}
				}
			}
		}

		LOG.warn("Query string not supplied, no results returned.");
		return null;
	}

	/**
	 * Appends an optional limit to the query if supplied by the user
	 */
	private static String limitQuery(final String sql, final Integer... limit) {
		String query = String.valueOf(sql);

		// limit the result set if a sample
		if (limit != null && limit.length == 1 && limit[0] != null) {
			query += String.format(" OFFSET 0 rows fetch next %d rows only", limit[0]);
		}
		return query;
	}

	/**
	 * Iterates through a query result set and dynamically assigns all the column results to a List of {@link DynaBean}s to be used
	 * in a templating context.
	 */
	private static List<DynaBean> parseResultSet(final ResultSet rs) throws Exception {
		final ArrayList<DynaBean> results = new ArrayList<DynaBean>();

		final ResultSetDynaClass rsdc = new ResultSetDynaClass(rs, false);
		final LazyDynaMap dynaMap = new LazyDynaMap(rsdc.getDynaProperties());

		final Iterator<DynaBean> rows = rsdc.iterator();
		while (rows.hasNext()) {
			final DynaBean oldRow = rows.next();
			final DynaBean newRow = dynaMap.newInstance();
			PropertyUtils.copyProperties(newRow, oldRow);
			results.add(newRow);
		}

		return results;
	}
}
