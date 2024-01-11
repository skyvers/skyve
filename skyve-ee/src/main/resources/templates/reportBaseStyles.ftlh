<style>
    @font-face {
        font-family: SourceSansPro;
        src: url(SourceSansPro-Regular.ttf);
    }
    @page {
        size: A4 portrait;
        margin: 1cm;
        
        @bottom-left {
            content: "${.now}";
            font-family: 'SourceSansPro', 'Arial', 'Microsoft Sans Serif', sans-serif;
            font-size: 10px;
            color: #333333;
        }
        
        @bottom-right {
            content: "Page " counter(page) " of " counter(pages);
            font-family: 'SourceSansPro', 'Arial', 'Microsoft Sans Serif', sans-serif;
            font-size: 10px;
            color: #333333;
        }
    }
    header {
        padding: 10px 0;
        margin-bottom: 20px;
    }
    
    header:before, header:after {
        content: "";
        display: table;
        clear: both;
    }
    header:after {
        clear: both;
    }
    h2.name {
        color: #111111;
        font-size: 1.4em;
        font-weight: normal;
        margin: 0;
    }
    body {
        font-family: 'SourceSansPro', 'Arial','Microsoft Sans Serif',sans-serif;
        color: #333333;
        font-size: 14px;
    }
    
    /* table reset */
	table, thead, tfoot, tr, th, td {
		margin: 0px; 
		padding: 0px; 
		box-sizing: border-box;
	}
	
	table {
		width: 100%;
	}
	table.table {
		margin-top: 20px;
		font-size: 13px;
		background:none;
		border:none;
		border-collapse:collapse;
		border-spacing:0;
		background-image:none;
	}
	table.table.full {
		font-size: 14px;
	}
	th, td {
		font-weight: unset;
		padding-right: 10px;
		line-height: 1.4;
		padding-top: 5px;
		padding-bottom: 5px;
	}
	tbody tr:nth-child(even) {
		background-color: #f6f6f6;
	}
	thead {
		font-weight: bold;
		background-color: #5C5D61;
		color: #fff;
	}
	thead th.title {
		text-align: left;
		text-transform: uppercase;
	}

	th.col1, td.col1 {
		padding-left: 10px;
	}
	
	.table td:nth-child(1), .table th:nth-child(1) {
		padding-left: 10px;
	}
    
    #footer {
        display: none;
    }
    #pagenumber:before {
        content: counter(page);
    }
    #pagecount:before {
        content: counter(pages);
    }
    
    .border-bottom-dashed {
        border-bottom: 1px solid #444;
        border-bottom-style: dashed;
    }
    .border-top-dashed {
        border-top: 1px solid #444;
        border-top-style: dashed;
    }
    
    .logo {
        float: left;
        margin-top: 8px;
    }
    .company, .reportTitle {
        float: right;
        text-align: right;
    }
    .centre, .center {
        text-align: center;
    }
    .right {
        text-align: right;
    }
    
    .col-1-12 {
        width: 8.33%;
    }
    .col-1-10 {
        width: 10%;
    }
    .col-1-9 {
        width: 11.11%;
    }
    .col-1-8 {
        width: 12%;
    }
    .col-1-6, .col-2-12 {
        width: 16.66%;
    }
    .col-1-5, .col-2-10 {
        width: 20%;
    }
    .col-1-4, .col-1-25, .col-3-12 {
        width: 25%;
    }
    .col-3-10 {
        width: 30%;
    }
    .col-1-3, .col-2-6, .col-4-12 {
        width: 33.33%;
    }
    .col-2-5, .col-4-10 {
        width: 40%;
    }
    .col-5-12 {
    	width: 41.66%;
    }
    .col-1-2, .col-5-10, .col-6-12, .col-1-50 {
        width: 50%;
    }
    .col-7-12 {
    	width: 58.33%;
    }
    .col-3-5 {
    	width: 60%;
    }
    .col-2-3, .col-8-12 {
        width: 66.66%;
    }
    .col-3-4, .col-9-12, .col-1-75 {
        width: 75%;
    }
    .col-4-5, .col-1-80 {
		width: 80%;
	}
    .col-10-12 {
    	width: 83.33%;
    }
    .col-11-12 {
    	width: 91.66%;
    }
    .col-1-1, .col-5-5, .col-12-12, .col-1-100 {
		width: 100%;
	}
	
	.page-break {
		page-break-after: always;
    }
	.page-break-avoid {
		page-break-inside: avoid;
	}
    
    .panel {
        background-color: #49A800;
        color: white;
    }
    
    /* repeat table headings across pages */
    .repeat-headings {
    	-fs-table-paginate: paginate;
    }
    
    .row {
        display: table-row;
        font-size: 90%;
        page-break-inside: avoid;
    }
    
    .row.full {
        font-size: 100%;
    }
    
    .row.child {
        font-size: 80%;
    }
    
    .row label {
        font-weight: bold;
    }
    
    @media screen {
        /* Factor out common styles for all of the "col-" classes.*/
        div[class^="col-"] {
            display: table-cell;
            padding: 5px;
        }
        
        #footer {
            display: block;
        }
        
        .panel {
            padding: 7px;
        }
        
        .items {
            clear: both;
            display: table;
            padding: 10px 2%;
            width: 96%;
        }
    }
    
    /* 	These styles are exactly like the screen styles except they use points (pt) as units of measure 
    instead of pixels (px) */
    @media print {
        div[class^="col-"] {
            display: table-cell;
            padding: 2pt 4pt;
        }
        
        .panel {
            padding: 7pt;
        }
        .company {
            font-size: 90%;
        }
        
        .items {
            clear: both;
            display: table;
            padding: 5pt 0;
            width: 100%;
        }
        
        .row {
            font-size: 80%;
        }
        
        .row.child {
            font-size: 70%;
        }
        
        .row.full {
            font-size: 90%;
        }
    }
</style>