package org.skyve.impl.bizport;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.util.Date;
import java.util.Map;

import org.skyve.CORE;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.messages.UploadException;
import org.skyve.domain.types.DateTime;
import org.skyve.domain.types.converters.Converter;
import org.supercsv.io.CsvMapReader;
import org.supercsv.prefs.CsvPreference;

public class CSVLoader extends AbstractDataFileLoader {
	private InputStreamReader fileReader;
	private CsvMapReader csvReader;
	private String[] headers;
	private Map<String, String> valueMap;

	public Map<String, String> getValueMap() {
		return valueMap;
	}

	public CSVLoader(LoaderActivityType activityType,
						InputStream fileInputStream,
						UploadException exception,
						String moduleName,
						String documentName,
						String... bindings) {

		super(activityType, exception, moduleName, documentName);
		addFields(bindings);

		fileReader = new InputStreamReader(fileInputStream, StandardCharsets.UTF_8);
		csvReader = new CsvMapReader(fileReader, CsvPreference.STANDARD_PREFERENCE);
		try {
			headers = csvReader.getHeader(true);
		}
		catch (IOException e) {
			throw new DomainException("Cannot read CSV header line", e);
		}

		dataIndex = 0;
	}

	@Override
	public void finalize() {
		try {
			if (csvReader != null) {
				csvReader.close();
			}
			if (fileReader != null) {
				fileReader.close();
			}
		}
		catch (IOException e) {
			throw new DomainException("Could not close the CSV stream", e);
		}
	}

	@Override
	public String getStringFieldValue(int index, boolean blankAsNull) {
		String hdr = headers[index];
		if (valueMap != null && valueMap.containsKey(hdr)) {
			String value = valueMap.get(hdr);
			if (blankAsNull && (value != null) && "".equals(value.trim())) {
				return null;
			}

			return value;
		}

		return null;
	}

	@Override
	public Date getDateFieldValue(int index) {
		String value = getStringFieldValue(index, true);
		if (null == value)
			return null;

		Converter<DateTime> converter = CORE.getCustomer().getDefaultDateTimeConverter();
		Date result;
		try {
			result = converter.fromDisplayValue(value);
		}
		catch (Exception e) {
			throw new DomainException("Cannot convert date/time from " + value, e);
		}
		return result;
	}

	@Override
	public Double getNumericFieldValue(int index, boolean emptyAsZero) {
		String value = getStringFieldValue(index, true);
		if (null == value)
			return Double.valueOf(0);

		return Double.valueOf(value);
	}

	@Override
	public void nextData() {
		Map<String, String> values;
		try {
			values = csvReader.read(headers);
		} 
		catch (IOException e) {
			throw new DomainException("Cannot read CSV line " + dataIndex, e);
		}		
		valueMap = values;
		dataIndex++;
	}

	@Override
	public boolean hasNextData() {
		return true;
	}

	@Override
	public boolean isNoData() {
		return (valueMap == null);
	}

	@Override
	public String getWhere(int index) {
		StringBuilder where = new StringBuilder(128);
		where.append("Line ").append(dataIndex);
		where.append(" ").append(headers[index]);
		where.append(".");
		return where.toString();
	}

	@Override
	public String debugData() {
		StringBuilder sb = new StringBuilder();
		sb.append("Line ").append(dataIndex);

		if (valueMap != null) {
			for (String key : valueMap.keySet()) {
				sb.append(", (").append(dataIndex).append(",").append(key).append(") = ");
				sb.append(valueMap.get(key).toString());
			}
		}
		else {
			sb.append(" Null");
		}
		return sb.toString();
	}
}
