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
		label	"X"
		graphics
		[
			x	97.5
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
			text	"X"
			fontSize	12
			fontName	"Dialog"
			model	"null"
		]
	]
	node
	[
		id	1
		label	"Y"
		graphics
		[
			x	127.5
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
			text	"Y"
			fontSize	12
			fontName	"Dialog"
			model	"null"
		]
	]
	node
	[
		id	2
		label	"Y"
		graphics
		[
			x	67.5
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
			text	"Y"
			fontSize	12
			fontName	"Dialog"
			model	"null"
		]
	]
	node
	[
		id	3
		label	"Ci"
		graphics
		[
			x	142.5
			y	209.5
			w	30.0
			h	30.0
			type	"ellipse"
			raisedBorder	0
			fill	"#CCFFCC"
			outline	"#000000"
		]
		LabelGraphics
		[
			text	"Ci"
			fontSize	12
			fontName	"Dialog"
			model	"null"
		]
	]
	node
	[
		id	4
		label	"Ci"
		graphics
		[
			x	67.5
			y	209.5
			w	30.0
			h	30.0
			type	"ellipse"
			raisedBorder	0
			fill	"#CCFFCC"
			outline	"#000000"
		]
		LabelGraphics
		[
			text	"Ci"
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
			y	306.5
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
			x	195.0
			y	306.5
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
		label	"._."
		graphics
		[
			x	15.0
			y	306.5
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
		id	8
		label	".T."
		graphics
		[
			x	75.0
			y	306.5
			w	30.0
			h	30.0
			type	"roundrectangle"
			raisedBorder	0
			fill	"#C0C0C0"
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
		source	1
		target	4
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
		source	2
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
		source	3
		target	5
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
		target	6
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
		source	4
		target	7
		graphics
		[
			style	"dashed"
			fill	"#000000"
			targetArrow	"standard"
		]
	]
	edge
	[
		source	4
		target	8
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
