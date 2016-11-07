Creator	"yFiles"
Version	"2.14"
graph
[
	hierarchic	1
	label	""
	directed	1
	node
	[
		id	0
		label	"x"
		graphics
		[
			x	75.0
			y	15.0
			w	30.0
			h	30.0
			type	"ellipse"
			raisedBorder	0
			fill	"#CCFFCC"
			outline	"#000000"
		]
		LabelGraphics
		[
			text	"x"
			fontSize	12
			fontName	"Dialog"
			model	"null"
		]
	]
	node
	[
		id	1
		label	"y"
		graphics
		[
			x	45.0
			y	112.0
			w	30.0
			h	30.0
			type	"ellipse"
			raisedBorder	0
			fill	"#CCFFCC"
			outline	"#000000"
		]
		LabelGraphics
		[
			text	"y"
			fontSize	12
			fontName	"Dialog"
			model	"null"
		]
	]
	node
	[
		id	2
		label	"y"
		graphics
		[
			x	105.0
			y	112.0
			w	30.0
			h	30.0
			type	"ellipse"
			raisedBorder	0
			fill	"#CCFFCC"
			outline	"#000000"
		]
		LabelGraphics
		[
			text	"y"
			fontSize	12
			fontName	"Dialog"
			model	"null"
		]
	]
	node
	[
		id	3
		label	"z"
		graphics
		[
			x	75.0
			y	210.0
			w	30.0
			h	30.0
			type	"ellipse"
			raisedBorder	0
			fill	"#CCFFCC"
			outline	"#000000"
		]
		LabelGraphics
		[
			text	"z"
			fontSize	12
			fontName	"Dialog"
			model	"null"
		]
	]
	node
	[
		id	4
		label	".T."
		graphics
		[
			x	15.0
			y	210.0
			w	30.0
			h	30.0
			type	"roundrectangle"
			raisedBorder	0
			fill	"#99CCFF"
			outline	"#000000"
		]
		LabelGraphics
		[
			text	".T."
			fontSize	12
			fontName	"Dialog"
			model	"null"
		]
	]
	node
	[
		id	5
		label	".T."
		graphics
		[
			x	135.0
			y	210.0
			w	30.0
			h	30.0
			type	"roundrectangle"
			raisedBorder	0
			fill	"#99CCFF"
			outline	"#000000"
		]
		LabelGraphics
		[
			text	".T."
			fontSize	12
			fontName	"Dialog"
			model	"null"
		]
	]
	node
	[
		id	6
		label	"._."
		graphics
		[
			x	45.0
			y	307.0
			w	30.0
			h	30.0
			type	"roundrectangle"
			raisedBorder	0
			fill	"#C0C0C0"
			outline	"#000000"
		]
		LabelGraphics
		[
			text	"._."
			fontSize	12
			fontName	"Dialog"
			model	"null"
		]
	]
	node
	[
		id	7
		label	".T."
		graphics
		[
			x	105.0
			y	307.0
			w	30.0
			h	30.0
			type	"roundrectangle"
			raisedBorder	0
			fill	"#99CCFF"
			outline	"#000000"
		]
		LabelGraphics
		[
			text	".T."
			fontSize	12
			fontName	"Dialog"
			model	"null"
		]
	]
	edge
	[
		source	0
		target	2
		graphics
		[
			style	"dashed"
			fill	"#000000"
			targetArrow	"standard"
		]
	]
	edge
	[
		source	0
		target	1
		label	"*"
		graphics
		[
			fill	"#000000"
			targetArrow	"standard"
		]
		LabelGraphics
		[
			text	"*"
			fontSize	12
			fontName	"Dialog"
			visible	0
			model	"six_pos"
			position	"tail"
		]
	]
	edge
	[
		source	1
		target	4
		graphics
		[
			style	"dashed"
			fill	"#000000"
			targetArrow	"standard"
		]
	]
	edge
	[
		source	1
		target	3
		label	"*"
		graphics
		[
			fill	"#000000"
			targetArrow	"standard"
		]
		LabelGraphics
		[
			text	"*"
			fontSize	12
			fontName	"Dialog"
			visible	0
			model	"six_pos"
			position	"tail"
		]
	]
	edge
	[
		source	2
		target	3
		graphics
		[
			style	"dashed"
			fill	"#000000"
			targetArrow	"standard"
		]
	]
	edge
	[
		source	2
		target	5
		label	"*"
		graphics
		[
			fill	"#000000"
			targetArrow	"standard"
		]
		LabelGraphics
		[
			text	"*"
			fontSize	12
			fontName	"Dialog"
			visible	0
			model	"six_pos"
			position	"tail"
		]
	]
	edge
	[
		source	3
		target	6
		graphics
		[
			style	"dashed"
			fill	"#000000"
			targetArrow	"standard"
		]
	]
	edge
	[
		source	3
		target	7
		label	"*"
		graphics
		[
			fill	"#000000"
			targetArrow	"standard"
		]
		LabelGraphics
		[
			text	"*"
			fontSize	12
			fontName	"Dialog"
			visible	0
			model	"six_pos"
			position	"tail"
		]
	]
]
