package org.skyve.wildcat.dataaccess.sql;

import java.math.BigDecimal;
import java.sql.Connection;
import java.sql.Date;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.sql.Time;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * This class wraps around a {@link PreparedStatement} and allows the programmer
 * to set parameters by name instead of by index. This eliminates any confusion
 * as to which parameter index represents what. This also means that rearranging
 * the SQL statement or adding a parameter doesn't involve renumbering your
 * indices. Code such as this:
 *
 * Connection con=getConnection();
 * String query="select * from my_table where name=? or address=?";
 * PreparedStatement p=con.prepareStatement(query); 
 * p.setString(1, "bob"); 
 * p.setString(2, "123 terrace ct");
 * ResultSet rs=p.executeQuery();
 *
 * can be replaced with:
 *
 * Connection con=getConnection(); 
 * String query="select * from my_table where name=:name or address=:address";
 * NamedParameterStatement p=new NamedParameterStatement(con, query); 
 * p.setString("name", "bob");
 * p.setString("address", "123 terrace ct"); 
 * ResultSet rs=p.executeQuery();
 *
 * @author adam_crume
 */
public class NamedParameterPreparedStatement implements AutoCloseable {
	/** The statement this object is wrapping. */
	private final PreparedStatement statement;

	/** Maps parameter names to arrays of ints which are the parameter indices. */
	private final Map<String, int[]> indexMap = new HashMap<>();

	/**
	 * Creates a NamedParameterStatement. Wraps a call to c.
	 * {@link Connection#prepareStatement(java.lang.String) prepareStatement}.
	 * 
	 * @param connection
	 *            the database connection
	 * @param query
	 *            the parameterized query
	 * @throws SQLException
	 *             if the statement could not be created
	 */
	public NamedParameterPreparedStatement(Connection connection, String query)
			throws SQLException {
		String parsedQuery = parse(query);
		statement = connection.prepareStatement(parsedQuery);
	}

	/**
	 * Parses a query with named parameters. The parameter-index mappings are
	 * put into the map, and the parsed query is returned. DO NOT CALL FROM
	 * CLIENT CODE. This method is non-private so JUnit code can test it.
	 * 
	 * @param query
	 *            query to parse
	 * @return the parsed query
	 */
	private String parse(String query) {
		Map<String, List<Integer>> tempMap = new HashMap<>();

		// I was originally using regular expressions, but they didn't work well
		// for ignoring parameter-like strings inside quotes.
		int length = query.length();
		StringBuilder parsedQuery = new StringBuilder(length);
		boolean inSingleQuote = false;
		boolean inDoubleQuote = false;
		int index = 1;
		for (int i = 0; i < length; i++) {
			char c = query.charAt(i);
			if (inSingleQuote) {
				if (c == '\'') {
					inSingleQuote = false;
				}
			}
			else if (inDoubleQuote) {
				if (c == '"') {
					inDoubleQuote = false;
				}
			}
			else {
				if (c == '\'') {
					inSingleQuote = true;
				}
				else if (c == '"') {
					inDoubleQuote = true;
				}
				else if ((c == ':') && 
							(i + 1 < length) && 
							Character.isJavaIdentifierStart(query.charAt(i + 1))) {
					int j = i + 2;
					while ((j < length) &&
							Character.isJavaIdentifierPart(query.charAt(j))) {
						j++;
					}
					String name = query.substring(i + 1, j);
					c = '?'; // replace the parameter with a question mark
					i += name.length(); // skip past the end if the parameter

					List<Integer> indexList = tempMap.get(name);
					if (indexList == null) {
						indexList = new ArrayList<>();
						tempMap.put(name, indexList);
					}
					indexList.add(new Integer(index));

					index++;
				}
			}
			parsedQuery.append(c);
		}

		// replace the lists of Integer objects with arrays of ints
		for (String name : tempMap.keySet()) {
			List<Integer> list = tempMap.get(name);
			int[] indexes = new int[list.size()];
			int i = 0;
			for (Integer x : list) {
				indexes[i++] = x.intValue();
			}
			indexMap.put(name, indexes);
		}

		return parsedQuery.toString();
    }

	/**
	 * Returns the indexes for a parameter.
	 * 
	 * @param name
	 *            parameter name
	 * @return parameter indexes
	 * @throws IllegalArgumentException
	 *             if the parameter does not exist
	 */
	private int[] getIndexes(String name) {
		int[] indexes = indexMap.get(name);
		if (indexes == null) {
			throw new IllegalArgumentException("Parameter not found: " + name);
		}
		return indexes;
	}


	/**
	 * Sets a parameter.
	 */
	public void setObject(String name, Object value) throws SQLException {
		int[] indexes = getIndexes(name);
		for (int i = 0; i < indexes.length; i++) {
			statement.setObject(indexes[i], value);
		}
	}

	/**
	 * Sets a null parameter.
	 */
	public void setNull(String name, int sqlType) throws SQLException {
		int[] indexes = getIndexes(name);
		for (int i = 0; i < indexes.length; i++) {
			statement.setNull(indexes[i], sqlType);
		}
	}
	
	/**
	 * Sets a parameter.
	 */
	public void setString(String name, String value) throws SQLException {
		int[] indexes = getIndexes(name);
		for (int i = 0; i < indexes.length; i++) {
			statement.setString(indexes[i], value);
		}
	}

	/**
	 * Sets a parameter.
	 */
	public void setBoolean(String name, boolean value) throws SQLException {
		int[] indexes = getIndexes(name);
		for (int i = 0; i < indexes.length; i++) {
			statement.setBoolean(indexes[i], value);
		}
	}

	/**
	 * Sets a parameter.
	 */
	public void setInt(String name, int value) throws SQLException {
		int[] indexes = getIndexes(name);
		for (int i = 0; i < indexes.length; i++) {
			statement.setInt(indexes[i], value);
		}
	}

	/**
	 * Sets a parameter.
	 */
	public void setLong(String name, long value) throws SQLException {
		int[] indexes = getIndexes(name);
		for (int i = 0; i < indexes.length; i++) {
			statement.setLong(indexes[i], value);
		}
	}

	/**
	 * Sets a parameter.
	 */
	public void setTimestamp(String name, Timestamp value) throws SQLException {
		int[] indexes = getIndexes(name);
		for (int i = 0; i < indexes.length; i++) {
			statement.setTimestamp(indexes[i], value);
		}
	}
	
	/**
	 * Sets a parameter
	 */
	public void setDate(String name, Date value) throws SQLException {
		int[] indexes = getIndexes(name);
		for (int i = 0; i < indexes.length; i++) {
			statement.setDate(indexes[i], value);
		}
	}

	/**
	 * Sets a parameter
	 */
	public void setTime(String name, Time value) throws SQLException {
		int[] indexes = getIndexes(name);
		for (int i = 0; i < indexes.length; i++) {
			statement.setTime(indexes[i], value);
		}
	}

	/**
	 * Sets a parameter
	 */
	public void setBigDecimal(String name, BigDecimal value) throws SQLException {
		int[] indexes = getIndexes(name);
		for (int i = 0; i < indexes.length; i++) {
			statement.setBigDecimal(indexes[i], value);
		}
	}

	/**
	 * Returns the underlying statement.
	 * 
	 * @return the statement
	 */
	public PreparedStatement getStatement() {
		return statement;
	}

	/**
	 * Executes the statement.
	 * 
	 * @return true if the first result is a {@link ResultSet}
	 * @throws SQLException
	 *             if an error occurred
	 * @see PreparedStatement#execute()
	 */
	public boolean execute() throws SQLException {
		return statement.execute();
	}

	/**
	 * Executes the statement, which must be a query.
	 * 
	 * @return the query results
	 * @throws SQLException
	 *             if an error occurred
	 * @see PreparedStatement#executeQuery()
	 */
	public ResultSet executeQuery() throws SQLException {
		return statement.executeQuery();
	}

	/**
	 * Executes the statement, which must be an SQL INSERT, UPDATE or DELETE
	 * statement; or an SQL statement that returns nothing, such as a DDL
	 * statement.
	 * 
	 * @return number of rows affected
	 * @throws SQLException
	 *             if an error occurred
	 * @see PreparedStatement#executeUpdate()
	 */
	public int executeUpdate() throws SQLException {
		return statement.executeUpdate();
	}

	/**
	 * Closes the statement.
	 * 
	 * @throws SQLException
	 *             if an error occurred
	 * @see Statement#close()
	 */
	@Override
	public void close() throws SQLException {
		statement.close();
	}

	/**
	 * Adds the current set of parameters as a batch entry.
	 * 
	 * @throws SQLException
	 *             if something went wrong
	 */
	public void addBatch() throws SQLException {
		statement.addBatch();
	}

	/**
	 * Executes all of the batched statements.
	 * 
	 * See {@link Statement#executeBatch()} for details.
	 * 
	 * @return update counts for each statement
	 * @throws SQLException
	 *             if something went wrong
	 */
	public int[] executeBatch() throws SQLException {
		return statement.executeBatch();
	}
}
