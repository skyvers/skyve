<#macro displayDocument document>
	<div class="page-break">
		<h3>${document.name}</h3>
		<p>${document.documentation!}</p>
		
		<table class="page-break-avoid">
		    <thead>
				<tr>
				    <th class="title" colspan="9">Attributes</th>
				</tr>
		        <tr>
		            <th>Attribute Name</th>
		            <th>Display Name</th>
		            <th>Type</th>
		            <th>Size</th>
		            <th>Required</th>
		            <th>Persistent</th>
		            <th>Description</th>
		            <th>Values</th>
		            <th>Deprecated</th>
		        </tr>
		    </thead>
		    <tbody>
		        <#list document.attributes as attribute>
		            <tr>
		                <td>${attribute.name}</td>
		                <td>${attribute.displayName!}</td>
		                <td>${attribute.type!}</td>
		                <td>${attribute.size!}</td>
		                <td>${attribute.required?c!}</td>
		                <td>${attribute.persistent?c!}</td>
		                <#if attribute.description?has_content>
		               		<td>${attribute.description?html}</td>
		                <#else>
		                	<td></td>
		                </#if>
		                <#if attribute.values?? && attribute.values?has_content>
						    <td>
						        <ul>
						            <#list attribute.values as value>
						                <li>${value?html}</li>
						            </#list>
						        </ul>
						    </td>
						<#else>
							<td></td>
						</#if>
						<td>${attribute.deprecated?c!}</td>       
		            </tr>
		        </#list>
		    </tbody>
		</table>
		
		<#if document.references?? && document.references?has_content>
		<table class="page-break-avoid">
		    <thead>
				<tr>
				    <th class="title" colspan="5">References</th>
				</tr>
		        <tr>
		            <th>Reference</th>
		            <th>Type</th>
		            <th>Document</th>
		            <th>Required</th>
		            <th>Deprecated</th>
		        </tr>
		    </thead>
		    <tbody>
		        <#list document.references as reference>
		            <tr>
		                <td>${reference.name}</td>
		                <td>${reference.type!}</td>
		                <td>${reference.document!}</td>
		                <td>${reference.required?c!}</td>
		                <td>${reference.deprecated?c!}</td>
		            </tr>
		        </#list>
		    </tbody>
		</table>
		</#if>
		
		<#if document.conditions?? && document.conditions?has_content>
		<table class="page-break-avoid">
		    <thead>
				<tr>
				    <th class="title" colspan="2">Conditions</th>
				</tr>
		        <tr>
		            <th>Name</th>
		            <th>Documentation</th>
		        </tr>
		    </thead>
		    <tbody>
		        <#list document.conditions as condition>
		            <tr>
		                <td>${condition.name}</td>
		                <td>${condition.documentation!}</td>
		            </tr>
		        </#list>
		    </tbody>
		</table>
		</#if>
		
		<#if document.constraints?? && document.constraints?has_content>
			<table class="page-break-avoid">
			    <thead>
					<tr>
					    <th class="title" colspan="4">UniqueConstraints</th>
					</tr>
			        <tr>
			            <th>Constraint</th>
			            <th>Description</th>
			            <th>Scope</th>
			            <th>References</th>
			        </tr>
			    </thead>
			    <tbody>
			        <#list document.constraints as constraint>
			            <tr>
			                <td>${constraint.name}</td>
			                <td>${constraint.description!}</td>
			                <td>${constraint.scope!}</td>
			                <td>${constraint.references!}</td>
			            </tr>
			        </#list>
			    </tbody>
			</table>
		</#if>
		
		<#if document.actions?? && document.actions?has_content>
		<h3>Actions</h3>
		<ul>
			<#list document.actions as action>
				<li>
					${action.name}
				</li>
			</#list>
		</ul>
		</#if>
	</div>
</#macro>

<#macro displayModule module>
<h2 class="name">${module.name}</h2>

<table class="page-break-avoid">
    <thead>
		<tr>
		    <th class="title" colspan="2">Documents</th>
		</tr>
        <tr>
            <th>Document Name</th>
            <th>Documentation</th>
        </tr>
    </thead>
    <tbody>
        <#list module.documents as doc>
            <tr>
                <td>${doc.name}</td>
                <td>${doc.documentation!}</td>
            </tr>
        </#list>
    </tbody>
</table>

<#if module.queries?? && module.queries?has_content>
	<table class="page-break-avoid">
	    <thead>
	    	<tr>
			    <th class="title" colspan="4">Documents</th>
			</tr>
	        <tr>
	            <th>Query</th>
	            <th>Driving Document</th>
	            <th>Description</th>
	            <th>Documentation</th>
	        </tr>
	    </thead>
	    <tbody>
	        <#list module.queries as query>
	            <tr>
	                <td>${query.name}</td>
	                <td>${query.drivingDocument!}</td>
	                <td>${query.description!}</td>
	                <td>${query.documentation!}</td>
	            </tr>
	        </#list>
	    </tbody>
	</table>
</#if>

<#if module.roles?? && module.roles?has_content>
	<table class="page-break-avoid">
	    <thead>
	    	<tr>
			    <th class="title" colspan="3">Roles</th>
			</tr>
	        <tr>
	            <th>Role</th>
	            <th>Description</th>
	            <th>Documentation</th>
	        </tr>
	    </thead>
	    <tbody>
	        <#list module.roles as role>
	            <tr>
	                <td>${role.name}</td>
	                <td>${role.descrption!}</td>
	                <td>${role.documentation!}</td>
	            </tr>
	        </#list>
	    </tbody>
	</table>
</#if>

<ul>
	<#list module.documents as document>
		<@displayDocument document />
	</#list>
</ul>
</#macro>


<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8" />
<title>${title}</title>
<#include "/reportBaseStyles.ftlh">
<style>
body {
	font-family: Arial, sans-serif;
	font-size: 10px;
}

h1 {
	color: #2A6595;
}

thead th.title {
	text-align: center;
}

th,td{
	padding: 10px;
}
table { width: 100%; border-collapse: collapse; margin-top: 20px; }
</style>
</head>
<body>
	<#assign modules=data.modules>

	<#list modules as module>
		<@displayModule module />
	</#list>
</body>
</html>