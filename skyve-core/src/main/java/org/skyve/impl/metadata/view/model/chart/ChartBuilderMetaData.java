package org.skyve.impl.metadata.view.model.chart;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElementRef;
import javax.xml.bind.annotation.XmlElementRefs;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.metadata.view.model.ModelMetaData;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.view.model.chart.Bucket;
import org.skyve.persistence.DocumentQuery.AggregateFunction;

@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE)
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE,
			propOrder = {"title",
							"label",
							"moduleName",
							"documentName",
							"categoryBinding",
							"categoryBucket",
							"valueBinding",
							"valueFunction",
							"top",
							"order"})
public class ChartBuilderMetaData implements ModelMetaData {
	private static final long serialVersionUID = -6525994383289095450L;

	private int modelIndex;
	private String title;
	private String label;
	private String moduleName;
	private String documentName;
	private String categoryBinding;
	private Bucket categoryBucket;
	private String valueBinding;
	private AggregateFunction valueFunction;
	private ChartBuilderTopMetaData top;
	private ChartBuilderOrderMetaData order;

	@Override
	public int getModelIndex() {
		return modelIndex;
	}

	@Override
	@XmlTransient
	public void setModelIndex(int modelIndex) {
		this.modelIndex = modelIndex;
	}
	
	public String getTitle() {
		return title;
	}
	@XmlAttribute(required = false)
	public void setTitle(String title) {
		this.title = title;
	}

	public String getLabel() {
		return label;
	}
	@XmlAttribute(required = true)
	public void setLabel(String label) {
		this.label = label;
	}
	
	public String getModuleName() {
		return moduleName;
	}
	@XmlAttribute(required = true)
	public void setModuleName(String moduleName) {
		this.moduleName = moduleName;
	}
	
	public String getDocumentName() {
		return documentName;
	}
	@XmlAttribute(required = true)
	public void setDocumentName(String documentName) {
		this.documentName = documentName;
	}
	
	public String getCategoryBinding() {
		return categoryBinding;
	}
	@XmlAttribute(required = true)
	public void setCategoryBinding(String categoryBinding) {
		this.categoryBinding = categoryBinding;
	}

	public Bucket getCategoryBucket() {
		return categoryBucket;
	}
	
	@XmlElementRefs({@XmlElementRef(type = NoBucketMetaData.class),
						@XmlElementRef(type = NumericMultipleBucketMetaData.class),
						@XmlElementRef(type = NumericRangeBucketMetaData.class),
						@XmlElementRef(type = TemporalBucketMetaData.class),
						@XmlElementRef(type = TextLengthBucketMetaData.class),
						@XmlElementRef(type = TextStartsWithBucketMetaData.class)})
	public void setCategoryBucket(Bucket categoryBucket) {
		this.categoryBucket = categoryBucket;
		if (categoryBucket instanceof NumericRangeBucketMetaData) {
			((NumericRangeBucketMetaData) categoryBucket).convert();
		}
	}

	public String getValueBinding() {
		return valueBinding;
	}
	@XmlAttribute(required = true)
	public void setValueBinding(String valueBinding) {
		this.valueBinding = valueBinding;
	}
	
	public AggregateFunction getValueFunction() {
		return valueFunction;
	}
	@XmlAttribute
	public void setValueFunction(AggregateFunction valueFunction) {
		this.valueFunction = valueFunction;
	}

	public ChartBuilderTopMetaData getTop() {
		return top;
	}
	@XmlElement(namespace = XMLMetaData.VIEW_NAMESPACE)
	public void setTop(ChartBuilderTopMetaData top) {
		this.top = top;
	}
	
	public ChartBuilderOrderMetaData getOrder() {
		return order;
	}
	@XmlElement(namespace = XMLMetaData.VIEW_NAMESPACE)
	public void setOrder(ChartBuilderOrderMetaData order) {
		this.order = order;
	}
}
