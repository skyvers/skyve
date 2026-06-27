package org.skyve.impl.tools.jasperreports;

import java.util.Map;

import net.sf.jasperreports.engine.JRDataset;
import net.sf.jasperreports.engine.JRException;
import net.sf.jasperreports.engine.JRQuery;
import net.sf.jasperreports.engine.JRValueParameter;
import net.sf.jasperreports.engine.JasperReportsContext;
import net.sf.jasperreports.engine.query.JRQueryExecuter;
import net.sf.jasperreports.engine.query.QueryExecuterFactory;

/**
 * Creates {@link SkyveQueryExecuter} instances for JasperReports datasets.
 */
public class SkyveQueryExecuterFactory implements QueryExecuterFactory {
	/**
	 * Creates a Skyve query executer from the dataset query text.
	 *
	 * @param jasperReportsContext report execution context
	 * @param dataset dataset that may contain a Skyve query expression
	 * @param parameters report parameters
	 * @return executer instance, or {@code null} when dataset query is missing
	 * @throws JRException if JasperReports reports an executer creation error
	 */
	@Override
	public JRQueryExecuter createQueryExecuter(JasperReportsContext jasperReportsContext, JRDataset dataset, Map<String, ? extends JRValueParameter> parameters) 
	throws JRException {
		if (dataset != null) {
			JRQuery query = dataset.getQuery();
			if (query != null) {
				return new SkyveQueryExecuter(query.getText());
			}
		}
		
		return null;
	}

	/**
	 * Returns built-in parameters supplied by this factory.
	 *
	 * @return always an empty array
	 */
	@Override
	public Object[] getBuiltinParameters() {
		return new Object[0];
	}

	/**
	 * Indicates that all query parameter types are accepted.
	 *
	 * @param className parameter type name
	 * @return always {@code true}
	 */
	@Override
	public boolean supportsQueryParameterType(String className) {
		return true;
	}
}
