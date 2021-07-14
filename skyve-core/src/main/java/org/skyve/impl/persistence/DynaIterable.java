package org.skyve.impl.persistence;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Iterator;

import org.apache.commons.beanutils.DynaBean;
import org.apache.commons.beanutils.ResultSetDynaClass;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.messages.SkyveException;
import org.skyve.domain.messages.TimeoutException;
import org.skyve.impl.persistence.hibernate.dialect.SkyveDialect;
import org.skyve.persistence.AutoClosingIterable;
import org.skyve.persistence.DataStore;

public class DynaIterable implements AutoClosingIterable<DynaBean> {
	private final NamedParameterPreparedStatement ps;
	private final ResultSet rs;
	
	public DynaIterable(Connection c, AbstractSQL sql, DataStore ds, SkyveDialect dialect) {
		try {
			ps = new NamedParameterPreparedStatement(c, sql.toQueryString());
			sql.prepareStatement(ps, ds, dialect);
			rs = ps.executeQuery();
		}
		catch (TimeoutException e) {
			throw e;
		}
		catch (SkyveException e) {
			throw e;
		}
		catch (Exception e) {
			throw new DomainException(e);
		}
	}

	@Override
	public Iterator<DynaBean> iterator() {
		try {
			final ResultSetDynaClass rsdc = new ResultSetDynaClass(rs, true, true);
			return rsdc.iterator();
		}
		catch (SQLException e) {
			throw new DomainException(e);
		}
	}

	@Override
	public void close() throws Exception {
		if ((rs != null) && (! rs.isClosed())) {
			rs.close();
		}
		if ((ps != null) && (! ps.isClosed())) {
			ps.close();
		}
	}
}
