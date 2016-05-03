package org.skyve.impl.tools.jasperreports;

import java.util.Map;

import net.sf.jasperreports.engine.JRDataset;
import net.sf.jasperreports.engine.JRException;
import net.sf.jasperreports.engine.JRQuery;
import net.sf.jasperreports.engine.query.JRQueryExecuter;
import net.sf.jasperreports.engine.query.JRQueryExecuterFactory;

@SuppressWarnings("deprecation")
public class SkyveQueryExecuterFactory implements JRQueryExecuterFactory {
	@Override
	@SuppressWarnings("rawtypes")
	public JRQueryExecuter createQueryExecuter(JRDataset dataset, Map parameters) 
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
