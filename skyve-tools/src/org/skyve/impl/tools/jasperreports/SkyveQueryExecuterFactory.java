package org.skyve.impl.tools.jasperreports;

import java.util.Map;

import net.sf.jasperreports.engine.JRDataset;
import net.sf.jasperreports.engine.JRException;
import net.sf.jasperreports.engine.JRQuery;
import net.sf.jasperreports.engine.JRValueParameter;
import net.sf.jasperreports.engine.JasperReportsContext;
import net.sf.jasperreports.engine.query.JRQueryExecuter;
import net.sf.jasperreports.engine.query.QueryExecuterFactory;

public class SkyveQueryExecuterFactory implements QueryExecuterFactory {
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

	@Override
	public Object[] getBuiltinParameters() {
		return new Object[0];
	}

	@Override
	public boolean supportsQueryParameterType(String className) {
		return true;
	}
}
