package org.skyve.domain.app.admin;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;

public class DomainAdminEnumTest {

	// ---- DataMaintenance.DataSensitivity ----

	@Test
	@SuppressWarnings("static-method")
	public void dataSensitivityToCode() {
		assertThat(DataMaintenance.DataSensitivity.none.toCode(), is("none"));
		assertThat(DataMaintenance.DataSensitivity.secret.toCode(), is("secret"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void dataSensitivityToDomainValue() {
		assertNotNull(DataMaintenance.DataSensitivity.confidential.toDomainValue());
		assertThat(DataMaintenance.DataSensitivity.confidential.toDomainValue().getCode(), is("confidential"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void dataSensitivityFromCode() {
		assertThat(DataMaintenance.DataSensitivity.fromCode("restricted"), is(DataMaintenance.DataSensitivity.restricted));
	}

	@Test
	@SuppressWarnings("static-method")
	public void dataSensitivityFromCodeMissing() {
		assertNull(DataMaintenance.DataSensitivity.fromCode("notACode"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void dataSensitivityToDomainValues() {
		assertNotNull(DataMaintenance.DataSensitivity.toDomainValues());
		assertEquals(6, DataMaintenance.DataSensitivity.toDomainValues().size());
	}

	@Test
	@SuppressWarnings("static-method")
	public void dataSensitivityValues() {
		assertEquals(6, DataMaintenance.DataSensitivity.values().length);
	}

	@Test
	@SuppressWarnings("static-method")
	public void dataSensitivityToLocalisedDescriptionReturnsNonNull() {
		assertNotNull(DataMaintenance.DataSensitivity.none.toLocalisedDescription());
	}

	@Test
	@SuppressWarnings("static-method")
	public void dataSensitivityFromLocalisedDescriptionFindsValue() {
		String desc = DataMaintenance.DataSensitivity.none.toLocalisedDescription();
		assertThat(DataMaintenance.DataSensitivity.fromLocalisedDescription(desc), is(DataMaintenance.DataSensitivity.none));
	}

	@Test
	@SuppressWarnings("static-method")
	public void dataSensitivityFromLocalisedDescriptionUnknownReturnsNull() {
		assertNull(DataMaintenance.DataSensitivity.fromLocalisedDescription("no such description"));
	}

	// ---- ReportParameter.Type ----

	@Test
	@SuppressWarnings("static-method")
	public void reportParameterTypeToCode() {
		assertThat(ReportParameter.Type.text.toCode(), is("text"));
		assertThat(ReportParameter.Type.integer.toCode(), is("integer"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void reportParameterTypeToDomainValue() {
		assertNotNull(ReportParameter.Type.date.toDomainValue());
		assertThat(ReportParameter.Type.date.toDomainValue().getCode(), is("date"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void reportParameterTypeFromCode() {
		assertThat(ReportParameter.Type.fromCode("long"), is(ReportParameter.Type.longInteger));
	}

	@Test
	@SuppressWarnings("static-method")
	public void reportParameterTypeFromCodeMissing() {
		assertNull(ReportParameter.Type.fromCode("notACode"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void reportParameterTypeToDomainValues() {
		assertNotNull(ReportParameter.Type.toDomainValues());
		assertEquals(4, ReportParameter.Type.toDomainValues().size());
	}

	@Test
	@SuppressWarnings("static-method")
	public void reportParameterTypeToLocalisedDescriptionReturnsNonNull() {
		assertNotNull(ReportParameter.Type.text.toLocalisedDescription());
	}

	@Test
	@SuppressWarnings("static-method")
	public void reportParameterTypeFromLocalisedDescriptionFindsValue() {
		String desc = ReportParameter.Type.text.toLocalisedDescription();
		assertThat(ReportParameter.Type.fromLocalisedDescription(desc), is(ReportParameter.Type.text));
	}

	@Test
	@SuppressWarnings("static-method")
	public void reportParameterTypeFromLocalisedDescriptionUnknownReturnsNull() {
		assertNull(ReportParameter.Type.fromLocalisedDescription("no such description"));
	}

	// ---- ReportDataset.DatasetType ----

	@Test
	@SuppressWarnings("static-method")
	public void reportDatasetTypeToCode() {
		assertThat(ReportDataset.DatasetType.bizQL.toCode(), is("BizQL"));
		assertThat(ReportDataset.DatasetType.SQL.toCode(), is("SQL"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void reportDatasetTypeToDomainValue() {
		assertNotNull(ReportDataset.DatasetType.constant.toDomainValue());
	}

	@Test
	@SuppressWarnings("static-method")
	public void reportDatasetTypeFromCode() {
		assertThat(ReportDataset.DatasetType.fromCode("SQL"), is(ReportDataset.DatasetType.SQL));
	}

	@Test
	@SuppressWarnings("static-method")
	public void reportDatasetTypeFromCodeMissing() {
		assertNull(ReportDataset.DatasetType.fromCode("notACode"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void reportDatasetTypeToDomainValues() {
		assertNotNull(ReportDataset.DatasetType.toDomainValues());
		assertEquals(4, ReportDataset.DatasetType.toDomainValues().size());
	}

	@Test
	@SuppressWarnings("static-method")
	public void reportDatasetTypeToLocalisedDescriptionReturnsNonNull() {
		assertNotNull(ReportDataset.DatasetType.SQL.toLocalisedDescription());
	}

	@Test
	@SuppressWarnings("static-method")
	public void reportDatasetTypeFromLocalisedDescriptionFindsValue() {
		String desc = ReportDataset.DatasetType.SQL.toLocalisedDescription();
		assertThat(ReportDataset.DatasetType.fromLocalisedDescription(desc), is(ReportDataset.DatasetType.SQL));
	}

	@Test
	@SuppressWarnings("static-method")
	public void reportDatasetTypeFromLocalisedDescriptionUnknownReturnsNull() {
		assertNull(ReportDataset.DatasetType.fromLocalisedDescription("no such description"));
	}
}
