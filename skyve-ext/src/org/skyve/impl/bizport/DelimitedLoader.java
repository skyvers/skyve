package org.skyve.impl.bizport;

import java.io.InputStream;
import java.io.InputStreamReader;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.logging.Level;

import org.skyve.bizport.BizPortException;
import org.skyve.util.Util;
import org.supercsv.cellprocessor.Optional;
import org.supercsv.cellprocessor.ift.CellProcessor;
import org.supercsv.io.CsvMapReader;
import org.supercsv.io.ICsvMapReader;
import org.supercsv.prefs.CsvPreference;

public class DelimitedLoader extends AbstractDataFileLoader {

	private ICsvMapReader mapReader = null;
	private String[] header;
	private CellProcessor[] processors;

	private String dateFormat;

	public DelimitedLoader(LoaderActivityType activityType, InputStream fileInputStream, BizPortException exception, 
			String moduleName, String documentName, char seperator, String dateFormat, String... bindings) throws Exception {
		
		super(activityType, exception, moduleName, documentName);
		addFields(bindings);
		
		CsvPreference preference = CsvPreference.STANDARD_PREFERENCE;
		if('\t' == seperator)
			preference = CsvPreference.TAB_PREFERENCE;
		
		mapReader = new CsvMapReader(new InputStreamReader(fileInputStream), preference);

		header = mapReader.getHeader(true);
		processors = new CellProcessor[bindings.length];
		for(int cntr = 0; cntr < bindings.length; ++cntr) {
			processors[cntr] = new Optional();
		}
		
		setDataIndex(0);
		this.dateFormat = dateFormat;
	}
	
	@Override
	public void finalize() {
		try {
			if (mapReader != null) {
				mapReader.close();
			}
		} catch (Exception e) {
			Util.LOGGER.log(Level.SEVERE, "Exception thrown closing input file");
		}
	}

	@Override
	public String getStringFieldValue(int fieldIndex, boolean blankAsNull) throws Exception {
		String hdr = header[fieldIndex];
		if(getValueMap().containsKey(hdr)) {
			String value = (String) getValueMap().get(hdr);
			if(blankAsNull && null != value && "".equals(value.trim())) {
				return null;
			} 
			
			return value;
		}

		return null;
	}

	@Override
	public Date getDateFieldValue(int fieldIndex) throws Exception {
		String value = getStringFieldValue(fieldIndex, true);
		if(null == value)
			return null;
		
		SimpleDateFormat sdf = new SimpleDateFormat(dateFormat);
		return sdf.parse(value);
	}

	@Override
	public Double getNumericFieldValue(int fieldIndex, boolean emptyAsZero) throws Exception {
		String value = getStringFieldValue(fieldIndex, true);
		if(null == value)
			return new Double(0);
		
		return Double.valueOf(value);
	}

	@Override
	public void nextData() throws Exception {
		setValueMap(mapReader.read(header, processors));
	}

	@Override
	public boolean hasNextData() throws Exception {
		return true;
	}


	@Override
	public boolean isNoData() throws Exception {
		return false;
	}

	@Override
	public String getWhere(int fieldIndex) throws Exception {
		StringBuilder where = new StringBuilder(128);
		where.append("Row ").append((getDataIndex() + 1));
		where.append(" column ").append(header[fieldIndex]);
		where.append(".");
		return where.toString();
	}

	@Override
	public String debugData() throws Exception {
		StringBuilder sb = new StringBuilder();
		sb.append("Row ").append(getDataIndex());
		
		if (valueMap != null) {
			for(String key : getValueMap().keySet()) {
				sb.append(", (").append(getDataIndex()).append(",").append(key).append(") = ");
				sb.append(valueMap.get(key).toString());
			}
		} else {
			sb.append(" Null");
		}
		return sb.toString();
	}

}
